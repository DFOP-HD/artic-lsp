#include "artic/ls/server.h"

#include <lsp/io/standardio.h>
#include <lsp/messages.h> // Generated message definitions

#include "artic/log.h"
#include "artic/ast.h"
#include <cctype>

#include "artic/ls/crash.h"
#include "artic/ls/workspace.h"
#include "lsp/types.h"
#include "fstream"

#ifndef ENABLE_JSON
#error("JSON support is required")
#endif


namespace artic::ls {

// Server ---------------------------------------------------------------------

Server::Server() 
    : connection_(lsp::Connection(lsp::io::standardIO()))
    , message_handler_(this->connection_)
{
    crash::setup_crash_handler();
    setup_events();
}

Server::~Server() = default;

void Server::send_message(const std::string& message, lsp::MessageType type) {
    message_handler_.sendNotification<lsp::notifications::Window_ShowMessage>(
        lsp::notifications::Window_ShowMessage::Params {
            .type = type,
            .message = message
        }
    );
}

// Server Events ----------------------------------------------------------------------

void Server::setup_events() {
    // Initilalize ----------------------------------------------------------------------
    message_handler_.add<lsp::requests::Initialize>([this](lsp::requests::Initialize::Params&& params) {
        log::debug( "Received Initialize request");
        
        std::filesystem::path workspace_root;
        std::filesystem::path workspace_config_path;
        std::filesystem::path global_config_path;

        // Extract workspace root from initialize params
        if (!params.rootUri.isNull()) {
            workspace_root = std::string(params.rootUri.value().path());
            
            if (params.initializationOptions.has_value()) {
                const auto& any = params.initializationOptions.value();
                if (any.isObject()) {
                    const auto& obj = any.object();
                    if (auto it = obj.find("workspaceConfig"); it != obj.end() && it->second.isString()) {
                        workspace_config_path = it->second.string();
                    }
                    if (auto it = obj.find("globalConfig"); it != obj.end() && it->second.isString()) {
                        global_config_path = it->second.string();
                    }
                }
            }

            if(workspace_config_path.empty()) send_message("No local artic.json workspace config", lsp::MessageType::Warning);
            if(global_config_path.empty())    send_message("No global artic.json config", lsp::MessageType::Warning);        
        } else {
            send_message("No workspace root provided in initialize request", lsp::MessageType::Error);
        }

        log::debug("Workspace root: {}", workspace_root);
        log::debug("Workspace config path: {}", workspace_config_path);
        log::debug("Global config path: {}", global_config_path);
        workspace_ = std::make_unique<workspace::Workspace>(workspace_root, workspace_config_path, global_config_path);
        
        return lsp::requests::Initialize::Result {
            .capabilities = lsp::ServerCapabilities{
                .textDocumentSync = lsp::TextDocumentSyncOptions{
                    .openClose = true,
                    .change    = lsp::TextDocumentSyncKind::None
                },
                .definitionProvider = true
            },
            .serverInfo = lsp::InitializeResultServerInfo {
                .name    = "Artic Language Server",
                .version = "0.1.0"
            }
        };
    });

    message_handler_.add<lsp::notifications::Initialized>([this](lsp::notifications::Initialized::Params&&){
        log::debug("Received Initialized notification");

        reload_workspace();
        // compile_files(workspace_->get_project_files());
    });

    // Shutdown ----------------------------------------------------------------------
    message_handler_.add<lsp::requests::Shutdown>([this]() {
        log::debug("Shutdown");
        running_ = false;
        return lsp::requests::Shutdown::Result {};
    });

    // Textdocument ----------------------------------------------------------------------

    message_handler_.add<lsp::notifications::TextDocument_DidChange>([](lsp::notifications::TextDocument_DidChange::Params&& params) {
        log::debug("LSP Document Changed");
    });
    message_handler_.add<lsp::notifications::TextDocument_DidClose>([](lsp::notifications::TextDocument_DidClose::Params&& params) {
        log::debug("LSP Document Closed");
    });
    message_handler_.add<lsp::notifications::TextDocument_DidOpen>([this](lsp::notifications::TextDocument_DidOpen::Params&& params) {
        log::debug("LSP Document Opened");
        auto path = std::string(params.textDocument.uri.path());
        if(auto proj = workspace_->project_for_file(path)){
            // known project
            compile_files(proj.value()->collect_files());
        } else {
            // default project
            log::debug("File not in workspace {}, using default project", path);
            
            auto files = workspace_->default_project()->collect_files();
            workspace::File out_of_project_file(path);
            out_of_project_file.read();
            files.push_back(&out_of_project_file);

            // TODO for maintainer
            // maybe file should not be a temporary here to avoid use after free errors
            // though i don't think file text should be accessed after compile_files.
            // However, you could imagine if we do incremental compilation / recompilation at a later stage,
            // log / locator could try to access the file text, which is likely stored as a string_view
            compile_files(files);
        }
    });
    message_handler_.add<lsp::notifications::TextDocument_DidSave>([](lsp::notifications::TextDocument_DidSave::Params&& params) {
        log::debug("LSP Document Saved");
    });

    // Workspace ----------------------------------------------------------------------

    // lsp::notifications::Workspace_DidChangeConfiguration
    message_handler_.add<lsp::notifications::Workspace_DidChangeConfiguration>([this](lsp::notifications::Workspace_DidChangeConfiguration::Params&& params) {
        log::debug("LSP Workspace DidChangeConfiguration: triggering config reload");
        // Optionally, could inspect params.settings to override paths.
        reload_workspace();
    });
    // lsp::notifications::Workspace_DidChangeWatchedFiles
    message_handler_.add<lsp::notifications::Workspace_DidChangeWatchedFiles>([this](lsp::notifications::Workspace_DidChangeWatchedFiles::Params&& params) {
        log::debug("LSP Workspace Did Change Watched Files");

        std::filesystem::path active_file;
        for(auto& change : params.changes) {
            auto path = change.uri.path();

            if ((!workspace_->workspace_config_path.empty() && path == workspace_->workspace_config_path) || 
                (!workspace_->global_config_path.empty()    && path == workspace_->global_config_path)) {
                log::debug("Configuration file change detected; reloading workspace");
                reload_workspace();
                return;
            }

            switch(change.type.index()) {
                case lsp::FileChangeType::Created: {
                    reload_workspace();
                    return;
                }
                case lsp::FileChangeType::Changed: {
                    // update file content
                    const auto& files = workspace_->projects_.tracked_files;
                    auto it = files.find(path);            
                    if(it != files.end()) {
                        it->second->read();
                    }

                    // set active
                    active_file = path;
                    break;
                }
                case lsp::FileChangeType::Deleted: {
                    reload_workspace();
                    return;
                }
                case lsp::FileChangeType::MAX_VALUE: break;
            }
        }
        
        compile_file(active_file);
    });

    // lsp::notifications::Workspace_DidChangeWorkspaceFolders
    // lsp::notifications::Workspace_DidCreateFiles
    // lsp::notifications::Workspace_DidDeleteFiles
    // lsp::notifications::Workspace_DidRenameFiles

    // Other ----------------------------------------------------------------------

    // lsp::requests::CallHierarchy_IncomingCalls
    // lsp::requests::CallHierarchy_OutgoingCalls
    // lsp::requests::Client_RegisterCapability
    // lsp::requests::Client_UnregisterCapability
    // lsp::requests::CodeAction_Resolve
    // lsp::requests::CodeLens_Resolve
    // lsp::requests::CompletionItem_Resolve
    // lsp::requests::DocumentLink_Resolve
    // lsp::requests::InlayHint_Resolve
    // lsp::requests::TextDocument_CodeAction
    // lsp::requests::TextDocument_CodeLens
    // lsp::requests::TextDocument_ColorPresentation
    // lsp::requests::TextDocument_Completion
    // lsp::requests::TextDocument_Declaration
    // lsp::requests::TextDocument_Definition
    message_handler_.add<lsp::requests::TextDocument_Definition>([this](lsp::requests::TextDocument_Definition::Params&& params) {
        // Go-to-definition using Locator from last compilation
        auto uri = params.textDocument.uri;
        auto file_path = std::string(uri.path());
        int req_row1 = static_cast<int>(params.position.line) + 1;      // Loc is 1-based
        int req_col1 = static_cast<int>(params.position.character) + 1; // Loc is 1-based

        if (!last_compilation_result_ || last_compilation_result_->stage < compiler::CompileResult::NameBinded) {
            return lsp::requests::TextDocument_Definition::Result{};
        }

        // Use Locator to extract the word at the requested position
        auto& locator = last_compilation_result_->compiler->locator;
        auto* info = locator.data(file_path);
        std::string ident;
        if (info) {
            const char* line_start = info->at(req_row1, 1);
            size_t line_len = info->line_size(req_row1);
            size_t col = req_col1;
            if (col < 1 || col > line_len) col = line_len;
            const char* cursor = info->at(req_row1, col);
            // Step left if not ident
            auto is_ident_char = [](char c) { return std::isalnum(static_cast<unsigned char>(c)) || c == '_'; };
            if (!is_ident_char(*cursor) && col > 1 && is_ident_char(*(cursor - 1))) {
                --cursor; --col;
            }
            if (!is_ident_char(*cursor)) {
                ident = "";
            } else {
                // Expand left
                const char* L = cursor;
                while (L > line_start && is_ident_char(*(L - 1))) --L;
                // Expand right
                const char* R = cursor;
                const char* line_end = line_start + line_len;
                while (R + 1 < line_end && is_ident_char(*(R + 1))) ++R;
                ident = std::string(L, R - L + 1);
            }
        }

        if (ident.empty()) {
            return lsp::requests::TextDocument_Definition::Result{};
        }

        // Find a matching top-level declaration by name
        const ast::NamedDecl* target = nullptr;
        for (const auto& decl : last_compilation_result_->program->decls) {
            if (auto* nd = decl->isa<ast::NamedDecl>()) {
                if (nd->id.name == ident) { target = nd; break; }
            }
        }

        if (!target || !target->loc.file) {
            return lsp::requests::TextDocument_Definition::Result{};
        }

        // Build LSP Location from target->loc
        auto def_uri = lsp::FileUri::fromPath(*target->loc.file);
        lsp::Location loc {
            .uri = def_uri,
            .range = lsp::Range{
                .start = lsp::Position{ static_cast<lsp::uint>(target->loc.begin.row - 1), static_cast<lsp::uint>(target->loc.begin.col - 1) },
                .end   = lsp::Position{ static_cast<lsp::uint>(target->loc.end.row   - 1), static_cast<lsp::uint>(target->loc.end.col   - 1) }
            }
        };
        return lsp::requests::TextDocument_Definition::Result{ loc };
    });
    // lsp::requests::TextDocument_Diagnostic
    // lsp::requests::TextDocument_DocumentColor
    // lsp::requests::TextDocument_DocumentHighlight
    // lsp::requests::TextDocument_DocumentLink
    // lsp::requests::TextDocument_DocumentSymbol
    // lsp::requests::TextDocument_FoldingRange
    // lsp::requests::TextDocument_Formatting
    // lsp::requests::TextDocument_Hover
    // lsp::requests::TextDocument_Implementation
    // lsp::requests::TextDocument_InlayHint
    // lsp::requests::TextDocument_InlineCompletion
    // lsp::requests::TextDocument_InlineValue
    // lsp::requests::TextDocument_LinkedEditingRange
    // lsp::requests::TextDocument_Moniker
    // lsp::requests::TextDocument_OnTypeFormatting
    // lsp::requests::TextDocument_PrepareCallHierarchy
    // lsp::requests::TextDocument_PrepareRename
    // lsp::requests::TextDocument_PrepareTypeHierarchy
    // lsp::requests::TextDocument_RangeFormatting
    // lsp::requests::TextDocument_RangesFormatting
    // lsp::requests::TextDocument_References
    // lsp::requests::TextDocument_Rename
    // lsp::requests::TextDocument_SelectionRange
    // lsp::requests::TextDocument_SemanticTokens_Full
    // lsp::requests::TextDocument_SemanticTokens_Full_Delta
    // lsp::requests::TextDocument_SemanticTokens_Range
    // lsp::requests::TextDocument_SignatureHelp
    // lsp::requests::TextDocument_TypeDefinition
    // lsp::requests::TextDocument_WillSaveWaitUntil
    // lsp::requests::TypeHierarchy_Subtypes
    // lsp::requests::TypeHierarchy_Supertypes
    // lsp::requests::Window_ShowDocument
    // lsp::requests::Window_ShowMessageRequest
    // lsp::requests::Window_WorkDoneProgress_Create
    // lsp::requests::Workspace_ApplyEdit
    // lsp::requests::Workspace_CodeLens_Refresh
    // lsp::requests::Workspace_Configuration
    // lsp::requests::Workspace_Diagnostic
    // lsp::requests::Workspace_Diagnostic_Refresh
    // lsp::requests::Workspace_ExecuteCommand
    // lsp::requests::Workspace_FoldingRange_Refresh
    // lsp::requests::Workspace_InlayHint_Refresh
    // lsp::requests::Workspace_InlineValue_Refresh
    // lsp::requests::Workspace_SemanticTokens_Refresh
    // lsp::requests::Workspace_Symbol
    // lsp::requests::Workspace_WillCreateFiles
    // lsp::requests::Workspace_WillDeleteFiles
    // lsp::requests::Workspace_WillRenameFiles
    // lsp::requests::Workspace_WorkspaceFolders
    // lsp::requests::WorkspaceSymbol_Resolve
    // lsp::notifications::CancelRequest
    // lsp::notifications::LogTrace
    // lsp::notifications::Progress
    // lsp::notifications::SetTrace
    // lsp::notifications::Exit
    // lsp::notifications::Initialized
    // lsp::notifications::NotebookDocument_DidChange
    // lsp::notifications::NotebookDocument_DidClose
    // lsp::notifications::NotebookDocument_DidOpen
    // lsp::notifications::NotebookDocument_DidSave
    // lsp::notifications::Telemetry_Event
    // lsp::notifications::TextDocument_PublishDiagnostics
    // lsp::notifications::TextDocument_WillSave
    // lsp::notifications::Window_LogMessage
    // lsp::notifications::Window_ShowMessage
    // lsp::notifications::Window_WorkDoneProgress_Cancel
}

// Run Server ----------------------------------------------------------------------

int Server::run() {
    running_ = true;
    
    try {
        log::debug("LSP Server starting...");
        
        while (running_) {
            try {
                message_handler_.processIncomingMessages();
                // std::this_thread::sleep_for(std::chrono::milliseconds(10));
            } catch (const std::exception& e) {
                log::error("LSP Message processing error: {}", e.what());
            }
        }
    } catch (const std::exception& e) {
        log::debug("LSP Server fatal error: {}", e.what());
        return 1;
    } catch (...) {
        log::debug("LSP Server unknown fatal error");
        return 1;
    }

    log::debug("LSP Server shutdown complete");
    return 0;
}

// Compilation ----------------------------------------------------------------------

static lsp::Array<lsp::Diagnostic> convert_diagnostics(const std::vector<Diagnostic>& diagnostics) {
    lsp::Array<lsp::Diagnostic> lsp_diagnostics;
    for (const auto& diag : diagnostics) {
        lsp::Diagnostic lsp_diag;
        lsp_diag.range = lsp::Range {
            .start = lsp::Position { static_cast<uint>(diag.loc.begin.row - 1), static_cast<uint>(diag.loc.begin.col - 1) },
            .end   = lsp::Position { static_cast<uint>(diag.loc.end.row   - 1), static_cast<uint>(diag.loc.end.col   - 1) }
        };
        lsp_diag.message = diag.message;
        switch (diag.severity) {
            case Diagnostic::Error:
                lsp_diag.severity = lsp::DiagnosticSeverity::Error;
                break;
            case Diagnostic::Warning:
                lsp_diag.severity = lsp::DiagnosticSeverity::Warning;
                break;
            case Diagnostic::Information:
                lsp_diag.severity = lsp::DiagnosticSeverity::Information;
                break;
            case Diagnostic::Hint:
                lsp_diag.severity = lsp::DiagnosticSeverity::Hint;
                break;
        }
        lsp_diagnostics.push_back(lsp_diag);
    }
    return lsp_diagnostics;
}

void Server::compile_files(const std::vector<const workspace::File*>& files){
    if (files.empty()) {
        log::error("no input files (compile_files)");
        return;
    }

    log::debug("Compiling {} provided file(s)", files.size());

    auto compiler = std::make_shared<compiler::CompilerInstance>();
    last_compilation_result_ = compiler::compile_files(compiler, files);
    compiler->log.print_summary();

    if(last_compilation_result_->stage == compiler::CompileResult::Valid){
        log::debug("Compile success");
    } else {
        log::debug("Compile failed");
    }

    // Send Diagnostics for the provided files only
    std::unordered_map<std::string, std::vector<Diagnostic>> diagnostics_by_file;
    for (const auto& diag : compiler->log.diagnostics) {
        diagnostics_by_file[*diag.loc.file].push_back(diag);
    }
    for (auto file : files) {
        auto it = diagnostics_by_file.find(file->path);
        auto path_str = file->path.string();
        const auto& diags = (it != diagnostics_by_file.end()) ? it->second : std::vector<Diagnostic>{};
        message_handler_.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(
            lsp::notifications::TextDocument_PublishDiagnostics::Params {
                .uri = lsp::FileUri::fromPath(path_str),
                .diagnostics = convert_diagnostics(diags)
            }
        );
    }
}

void Server::compile_file(const std::filesystem::path& file){

}

void Server::reload_workspace(const std::string& active_file) {
    log::debug("Reloading workspace configuration");
    workspace::WorkspaceConfigLog log;
    workspace_->reload(log);
    // Clear previous diagnostics for project files
    auto _ = message_handler_.sendRequest<lsp::requests::Workspace_Diagnostic_Refresh>();

    
    auto publish = [&](const std::string& path){
        if (path.empty()) return;
        std::vector<lsp::Diagnostic> diags;
        auto makeDiag = [&](const workspace::WorkspaceConfigLog::Message& msg, lsp::DiagnosticSeverity sev, std::vector<lsp::Diagnostic>& diags) {
            lsp::Diagnostic diag;
            diag.message = msg.message;
            diag.severity = sev;
            
            if (msg.context.has_value()) {
                const auto& ctx = msg.context.value();
                std::ifstream file(path);
                int ctx_count = 0;
                if (file) {
                    std::string line;
                    lsp::uint row = 0;
                    while (std::getline(file, line)) {
                        size_t pos = 0;
                        while ((pos = line.find(ctx.literal, pos)) != std::string::npos) {
                            lsp::Diagnostic pos_diag(diag);
                            pos_diag.range = lsp::Range{
                                lsp::Position{row, static_cast<lsp::uint>(pos)},
                                lsp::Position{row, static_cast<lsp::uint>(pos + ctx.literal.size())}
                            };
                            diags.push_back(pos_diag);
                            ctx_count++;
                            pos += ctx.literal.size();
                        }
                        ++row;
                    }
                }
                if(ctx_count > 0) return;
            }
            
            diag.range = lsp::Range{ lsp::Position{0,0}, lsp::Position{0,0} };
            
            diags.push_back(diag);
        };
        for (auto& e : log.errors) makeDiag(e, lsp::DiagnosticSeverity::Error, diags);
        for (auto& w : log.warnings) makeDiag(w, lsp::DiagnosticSeverity::Warning, diags);
        message_handler_.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(
            lsp::notifications::TextDocument_PublishDiagnostics::Params {
                .uri = lsp::FileUri::fromPath(path),
                .diagnostics = diags
            }
        );
    };
    publish(workspace_->workspace_config_path);
    publish(workspace_->global_config_path);
    
    bool print_to_console = false;
    if(print_to_console){
        log::debug("Reloaded Workspace with {} errors and {} warnings", log.errors.size(), log.warnings.size());
        for (auto& e : log.errors) std::clog << "Error: " << e.message << std::endl;
        for (auto& w : log.warnings) std::clog << "Warning: " << w.message << std::endl;
        workspace_->projects_.print();
    }
}

} // namespace artic::ls