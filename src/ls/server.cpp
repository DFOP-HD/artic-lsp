#include "artic/ls/server.h"

#include <lsp/io/standardio.h>
#include <lsp/messages.h> // Generated message definitions

#include "artic/log.h"
#include "artic/ast.h"
#include <cctype>

#include "artic/ls/crash.h"


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
        
        // Extract workspace root from initialize params
        if (!params.rootUri.isNull()) {
            workspace_root_ = params.rootUri.value().path();
            log::debug("Workspace root: {}", workspace_root_);
            
            // Discover project files
            workspace_.load_from_config(workspace_root_);
        } else {
            send_message("No workspace root provided in initialize request", lsp::MessageType::Error);
        }

        compile_files(workspace_.get_project_files());
        
        return lsp::requests::Initialize::Result {
            .capabilities = lsp::ServerCapabilities{
                .textDocumentSync = lsp::TextDocumentSyncOptions{
                    .openClose = true,
                    .change    = lsp::TextDocumentSyncKind::Full
                },
                .definitionProvider = true
            },
            .serverInfo = lsp::InitializeResultServerInfo {
                .name    = "Artic Language Server",
                .version = "0.1.0"
            }
        };
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
        // If the opened file is already tracked by the workspace, compile the workspace; otherwise compile only this file
        bool in_workspace = false;
        for (const auto& f : workspace_.get_project_files()) {
            if (f.path == path) { in_workspace = true; break; }
        }

        if (in_workspace) {
            compile_files(workspace_.get_project_files());
        } else {
            File single{path};
            log::debug("File not in workspace {}", single.path);
            single.text = params.textDocument.text; // content sent on open
            compile_files({std::move(single)});
        }
    });
    message_handler_.add<lsp::notifications::TextDocument_DidSave>([](lsp::notifications::TextDocument_DidSave::Params&& params) {
        log::debug("LSP Document Saved");
    });

    // Workspace ----------------------------------------------------------------------

    // lsp::notifications::Workspace_DidChangeConfiguration
    // lsp::notifications::Workspace_DidChangeWatchedFiles
    message_handler_.add<lsp::notifications::Workspace_DidChangeWatchedFiles>([this](lsp::notifications::Workspace_DidChangeWatchedFiles::Params&& params) {
        log::debug("LSP Workspace Did Change Watched Files");

        for(auto& change : params.changes) {
            switch(change.type.index()) {
                case lsp::FileChangeType::Created:
                    workspace_.handle_file_created(change.uri.path());
                    break;
                case lsp::FileChangeType::Changed:
                    workspace_.handle_file_changed(change.uri.path());
                    break;
                case lsp::FileChangeType::Deleted:
                    workspace_.handle_file_deleted(change.uri.path());
                    break;
                case lsp::FileChangeType::MAX_VALUE:                    break;
            }
        }
        // If the changed file is part of the workspace, compile the whole workspace; otherwise compile only that file
        if (!params.changes.empty()) {
            auto changed_path = std::string(params.changes.front().uri.path());
            bool in_workspace = false;
            for (const auto& f : workspace_.get_project_files()) {
                if (f.path == changed_path) { in_workspace = true; break; }
            }
            if (in_workspace) {
                compile_files(workspace_.get_project_files());
            } else {
                std::vector<File> files;
                File single{changed_path};
                single.read();
                files.emplace_back(std::move(single));
                compile_files(files);
            }
        } else {
            compile_files(workspace_.get_project_files());
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

void Server::compile_files(const std::vector<File>& files){
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
    for (const auto& file : files) {
        auto it = diagnostics_by_file.find(file.path);
        const auto& diags = (it != diagnostics_by_file.end()) ? it->second : std::vector<Diagnostic>{};
        message_handler_.sendNotification<lsp::notifications::TextDocument_PublishDiagnostics>(
            lsp::notifications::TextDocument_PublishDiagnostics::Params {
                .uri = lsp::FileUri::fromPath(file.path),
                .diagnostics = convert_diagnostics(diags)
            }
        );
    }
}

} // namespace artic::ls