#include "artic/ls/server.h"

#include <lsp/io/standardio.h>
#include <lsp/messages.h> // Generated message definitions

#include "artic/log.h"
#include "artic/ast.h"
#include <cctype>
#include <unordered_set>

#include "artic/ls/crash.h"
#include "artic/ls/workspace.h"
#include "lsp/types.h"
#include <fstream>
#include <string_view>

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


struct InitializeData {
    std::filesystem::path workspace_root;
    std::filesystem::path workspace_config_path;
    std::filesystem::path global_config_path;
};

static InitializeData parse_initialize_options(const lsp::requests::Initialize::Params& params, Server& log) {
    InitializeData data;

    if (!params.rootUri.isNull()) {
        data.workspace_root = std::string(params.rootUri.value().path());
        
        if (params.initializationOptions.has_value()) {
            const auto& any = params.initializationOptions.value();
            if (any.isObject()) {
                const auto& obj = any.object();
                if (auto it = obj.find("workspaceConfig"); it != obj.end() && it->second.isString()) {
                    data.workspace_config_path = it->second.string();
                }
                if (auto it = obj.find("globalConfig"); it != obj.end() && it->second.isString()) {
                    data.global_config_path = it->second.string();
                }
            }
        } else {
            log.send_message("No workspace root provided in initialize request", lsp::MessageType::Error);
        }
    } else {
        log.send_message("No root URI provided in initialize request", lsp::MessageType::Error);
    }
    return data;
}

Server::FileType Server::get_file_type(const std::filesystem::path& file) {
    if(file.extension() == ".json")
        return Config;
    return Source;
}

// Server Events ----------------------------------------------------------------------

void Server::setup_events() {
    // Initilalize ----------------------------------------------------------------------
    message_handler_.add<lsp::requests::Initialize>([this](lsp::requests::Initialize::Params&& params) {
        log::debug( "LSP >>> Initialize");
        
        InitializeData init_data = parse_initialize_options(params, *this);

        if(init_data.workspace_config_path.empty()) send_message("No local artic.json workspace config", lsp::MessageType::Warning);
        if(init_data.global_config_path.empty())    send_message("No global artic.json config", lsp::MessageType::Warning); 
        log::debug("Workspace root: {}", init_data.workspace_root);
        log::debug("Workspace config path: {}", init_data.workspace_config_path);
        log::debug("Global config path: {}", init_data.global_config_path);
        workspace_ = std::make_unique<workspace::Workspace>(
            init_data.workspace_root, 
            init_data.workspace_config_path, 
            init_data.global_config_path
        );
        
        return lsp::requests::Initialize::Result {
            .capabilities = lsp::ServerCapabilities{
                .textDocumentSync = lsp::TextDocumentSyncOptions{
                    .openClose = true,
                    .change    = lsp::TextDocumentSyncKind::Incremental,
                    .save      = lsp::SaveOptions{ .includeText = false },
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
        log::debug("LSP >>> Initialized");
        reload_workspace();
    });

    // Shutdown ----------------------------------------------------------------------
    message_handler_.add<lsp::requests::Shutdown>([this]() {
        log::debug("LSP >>> Shutdown");
        running_ = false;
        return lsp::requests::Shutdown::Result {};
    });

    // Textdocument ----------------------------------------------------------------------
    message_handler_.add<lsp::notifications::TextDocument_DidChange>([](lsp::notifications::TextDocument_DidChange::Params&& params) {
        log::debug("LSP >>> TextDocument DidChange");
    });
    message_handler_.add<lsp::notifications::TextDocument_DidClose>([](lsp::notifications::TextDocument_DidClose::Params&& params) {
        log::debug("LSP >>> TextDocument DidClose");
    });
    message_handler_.add<lsp::notifications::TextDocument_DidOpen>([this](lsp::notifications::TextDocument_DidOpen::Params&& params) {
        log::debug("LSP >>> TextDocument DidOpen");

        if(get_file_type(params.textDocument.uri.path()) == FileType::Source) {
            auto path = std::string(params.textDocument.uri.path());
            compile_file(path);
        }
    });
    message_handler_.add<lsp::notifications::TextDocument_DidSave>([this](lsp::notifications::TextDocument_DidSave::Params&& params) {
        log::debug("LSP >>> TextDocument DidSave");
        if(get_file_type(params.textDocument.uri.path()) == FileType::Config) {
            reload_workspace();
            return;
        }
        auto file = params.textDocument.uri.path();
        const auto& files = workspace_->projects_.tracked_files;
        auto it = files.find(file);
        if(it != files.end()) {
            it->second->read();
        }
        compile_file(file);;
    });

    // Workspace ----------------------------------------------------------------------

    message_handler_.add<lsp::notifications::Workspace_DidChangeConfiguration>([this](lsp::notifications::Workspace_DidChangeConfiguration::Params&& params) {
        log::debug("LSP >>> Workspace DidChangeConfiguration");
        // Optionally, could inspect params.settings to override paths.
        reload_workspace();
    });
    message_handler_.add<lsp::notifications::Workspace_DidChangeWatchedFiles>([this](lsp::notifications::Workspace_DidChangeWatchedFiles::Params&& params) {
        log::debug("LSP >>> Workspace DidChangeWatchedFiles");

        for(auto& change : params.changes) {
            auto path = change.uri.path();

            switch(change.type.index()) {
                case lsp::FileChangeType::Created: 
                case lsp::FileChangeType::Deleted: {
                    reload_workspace();
                    return;
                }
                case lsp::FileChangeType::Changed: break; // Handle elsewhere
                case lsp::FileChangeType::MAX_VALUE: break;
            }
        }
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
    message_handler_.add<lsp::requests::TextDocument_Definition>([this](lsp::requests::TextDocument_Definition::Params&& params) {
        log::debug("LSP >>> TextDocument Definition");
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
            case Diagnostic::Error:   lsp_diag.severity = lsp::DiagnosticSeverity::Error;       break;
            case Diagnostic::Warning: lsp_diag.severity = lsp::DiagnosticSeverity::Warning;     break;
            case Diagnostic::Info:    lsp_diag.severity = lsp::DiagnosticSeverity::Information; break;
            case Diagnostic::Hint:    lsp_diag.severity = lsp::DiagnosticSeverity::Hint;        break;
        }
        lsp_diagnostics.push_back(lsp_diag);
    }
    return lsp_diagnostics;
}

void Server::compile_files(std::span<const workspace::File*> files){
    if (files.empty()) {
        log::error("no input files (compile_files)");
        return;
    }

    log::debug("Compiling {} file(s)", files.size());

    auto compiler = std::make_shared<compiler::CompilerInstance>();
    last_compilation_result_ = compiler::compile_files(files, compiler);

    const bool print_compile_log = false;
    if(print_compile_log) compiler->log.print_summary();

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
    if(auto proj = workspace_->project_for_file(file)){
        // known project
        auto files = proj.value()->collect_files();
        log::debug("Compiling file {} (project '{}')", file, proj.value()->name);
        compile_files(files);
    } else {
        auto default_proj = workspace_->default_project();
        // default project
        log::debug("Compiling file {} (not in workspace -> using default project {})", file, default_proj->name);
        
        auto files = default_proj->collect_files();
        workspace::File out_of_project_file(file);
        out_of_project_file.read();
        files.push_back(&out_of_project_file);

        // TODO for maintainer
        // maybe file should not be a temporary here to avoid use after free errors
        // though i don't think file text should be accessed after compile_files.
        // However, you could imagine if we do incremental compilation / recompilation at a later stage,
        // log / locator could try to access the file text, which is likely stored as a string_view
        compile_files(files);
    }
}

std::vector<lsp::Range> find_in_file(std::filesystem::path const& file, std::string_view literal){
    std::vector<lsp::Range> ranges;
    if(literal.empty()) return ranges;
    std::ifstream ifs(file);
    if (!ifs) return ranges;

    std::string line;
    lsp::uint line_number = 0;
    while (std::getline(ifs, line)) {
        size_t pos = line.find(literal);
        while (pos != std::string::npos) {
            ranges.push_back(lsp::Range{
                lsp::Position{line_number, static_cast<lsp::uint>(pos)},
                lsp::Position{line_number, static_cast<lsp::uint>(pos + literal.size())}
            });
            pos = line.find(literal, pos + 1);
        }
        line_number++;
    }
    return ranges;
}

void Server::publish_diagnostics(const workspace::ConfigLog& log) {
    // Clear previous diagnostics for project files
    using FileDiags = std::unordered_map<std::filesystem::path, std::vector<lsp::Diagnostic>>;
    
    FileDiags fileDiags;
    auto makeDiag = [&](
        const workspace::ConfigLog::Message& msg, 
        FileDiags& diags, 
        std::optional<std::filesystem::path> display_in_include_of_other_file = std::nullopt
    ) {
        lsp::Diagnostic diag;
        diag.message = msg.message;
        diag.severity = msg.severity;
        diag.range = lsp::Range{ lsp::Position{0,0}, lsp::Position{0,0} };
        auto file = msg.file;
        if(display_in_include_of_other_file) file = display_in_include_of_other_file.value();

        int display_count = 0;
        if(msg.context.has_value()) {
            auto literal = msg.context.value().literal;
            if(display_in_include_of_other_file) literal = "include-projects";

            auto occurrences = find_in_file(file, literal);
            for(auto& occ : occurrences) {
                lsp::Diagnostic pos_diag(diag);
                pos_diag.range = occ;
                diags[file].push_back(pos_diag);
                display_count++;
            }
        }
        if(display_count == 0) diags[file].push_back(diag);
    };

    // create diagnostics
    for (auto& e : log.messages) {
        makeDiag(e, fileDiags);
        if(e.severity == lsp::DiagnosticSeverity::Error) 
            makeDiag(e, fileDiags, workspace_->workspace_config_path);
    }

    // Send diagnostics
    for(auto& [file, diags] : fileDiags) {
        message_handler_.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(
            lsp::notifications::TextDocument_PublishDiagnostics::Params {
                .uri = lsp::FileUri::fromPath(file.string()),
                .diagnostics = diags
            }
        );
    }
}

void Server::reload_workspace(const std::string& active_file) {
    log::debug("Reloading workspace configuration");
    workspace::ConfigLog log;
    workspace_->reload(log);

    bool print_to_console = true;
    if(print_to_console) {
        log::debug("Reloaded Workspace");
        log::debug("--- Config Log ---");
        for (auto& e : log.messages) {
            if(e.severity > lsp::DiagnosticSeverity::Warning) continue;
            auto s = 
                (e.severity == lsp::DiagnosticSeverity::Error)        ? "Error" :
                (e.severity == lsp::DiagnosticSeverity::Warning)      ? "Warning" : 
                (e.severity == lsp::DiagnosticSeverity::Information)  ? "Info" : 
                (e.severity == lsp::DiagnosticSeverity::Hint)         ? "Hint" : 
                                                                        "Unknown";

            log::debug("[{}] {}: {}", s, e.file, e.message);
        }
        log::debug("--- Config Log ---");
        workspace_->projects_.print();
    }

    // This is somehow blocking. TODO investigate
    publish_diagnostics(log);
}

} // namespace artic::ls