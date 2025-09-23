#include "artic/ls/server.h"

#include "artic/ls/config.h"
#include "artic/ls/crash.h"
#include "artic/ls/workspace.h"
#include "artic/log.h"
#include "artic/ast.h"

#include <lsp/types.h>
#include <lsp/io/standardio.h>
#include <lsp/messages.h>

#include <fstream>
#include <string_view>
#include <cctype>


#ifndef ENABLE_JSON
#error("JSON support is required")
#endif

namespace reqst = lsp::requests;
namespace notif = lsp::notifications;

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

int Server::run() {
    log::info("LSP Server starting...");
    try {        
        running_ = true;
        while (running_) {
            try {
                message_handler_.processIncomingMessages();
                // std::this_thread::sleep_for(std::chrono::milliseconds(10));
            } catch (const std::exception& e) {
                log::info("LSP Message processing error: {}", e.what());
            }
        }
    } catch (...) {
        log::info("LSP Server unknown fatal error");
        return 1;
    }

    log::info("LSP Server shutdown complete");
    return 0;
}

void Server::send_message(const std::string& message, lsp::MessageType type) {
    message_handler_.sendNotification<notif::Window_ShowMessage>({ .type = type, .message = message });
}

Server::FileType Server::get_file_type(const std::filesystem::path& file) {
    return file.extension() == ".json" ? Config : Source;
}

// Server Compilation / Diagnostics ----------------------------------------------------------------------

void Server::compile_files(std::span<const workspace::File*> files){
    if (files.empty()) {
        log::info("no input files (compile_files)");
        return;
    }

    log::info("Compiling {} file(s)", files.size());

    auto compiler = std::make_shared<compiler::CompilerInstance>();
    last_compile = compiler->compile_files(files);
    last_compile->compiler = compiler;

    const bool print_compile_log = false;
    if(print_compile_log) compiler->log.print_summary();

    if(last_compile->stage == compiler::CompileResult::Valid){
        log::info("Compile success");
    } else {
        log::info("Compile failed");
    }

    auto convert_diagnostic = [](const Diagnostic& diag) {
        lsp::Diagnostic lsp_diag;
        lsp_diag.message = diag.message;
        lsp_diag.range = lsp::Range {
            .start = lsp::Position { static_cast<uint>(diag.loc.begin.row - 1), static_cast<uint>(diag.loc.begin.col - 1) },
            .end   = lsp::Position { static_cast<uint>(diag.loc.end.row   - 1), static_cast<uint>(diag.loc.end.col   - 1) }
        };
        switch (diag.severity) {
            case Diagnostic::Error:   lsp_diag.severity = lsp::DiagnosticSeverity::Error;       break;
            case Diagnostic::Warning: lsp_diag.severity = lsp::DiagnosticSeverity::Warning;     break;
            case Diagnostic::Info:    lsp_diag.severity = lsp::DiagnosticSeverity::Information; break;
            case Diagnostic::Hint:    lsp_diag.severity = lsp::DiagnosticSeverity::Hint;        break;
        }
        return lsp_diag;
    };

    // Send Diagnostics for the provided files only
    std::unordered_map<std::string, std::vector<lsp::Diagnostic>> diagnostics_by_file;
    for (const auto& diag : compiler->log.diagnostics) {
        diagnostics_by_file[*diag.loc.file].push_back(convert_diagnostic(diag));
    }
    for (const auto* file : files) {
        auto path = file->path.string();

        message_handler_.sendNotification<notif::TextDocument_PublishDiagnostics>(
            notif::TextDocument_PublishDiagnostics::Params {
                .uri = lsp::FileUri::fromPath(path),
                .diagnostics = diagnostics_by_file.contains(path) ? diagnostics_by_file.at(path) : std::vector<lsp::Diagnostic>{}
            }
        );
    }
}

void Server::compile_file(const std::filesystem::path& file){
    if(auto proj = workspace_->project_for_file(file)){
        // known project    
        log::info("Compiling file {} (project '{}')", file, proj.value()->name);

        auto files = proj.value()->collect_files();
        compile_files(files);
    } else {
        // default project
        auto default_proj = workspace_->default_project();
        
        log::info("Compiling file {} (not in workspace -> using default project {})", file, default_proj->name);
        
        auto files = default_proj->collect_files();
        auto temp_file = std::make_unique<workspace::File>(file);
        temp_file->read();
        files.push_back(temp_file.get());

        compile_files(files);

        // keep the temporary file alive for diagnostics
        if(last_compile) last_compile->temporary_files.push_back(std::move(temp_file));
    }
}

// Server Reload Workspace ----------------------------------------------------------------------

void Server::reload_workspace(const std::string& active_file) {
    log::info("Reloading workspace configuration");
    workspace::config::ConfigLog log;
    workspace_->reload(log);
    // This is somehow blocking. TODO investigate
    publish_config_diagnostics(log);
}

static inline std::vector<lsp::Range> find_in_file(std::filesystem::path const& file, std::string_view literal){
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

void Server::publish_config_diagnostics(const workspace::config::ConfigLog& log) {
    const bool print_to_console = true;
    if(print_to_console) {
        log::info("Reloaded Workspace");
        log::info("--- Config Log ---");
        for (auto& e : log.messages) {
            if(e.severity > lsp::DiagnosticSeverity::Warning) continue;
            auto s = 
                (e.severity == lsp::DiagnosticSeverity::Error)        ? "Error" :
                (e.severity == lsp::DiagnosticSeverity::Warning)      ? "Warning" : 
                (e.severity == lsp::DiagnosticSeverity::Information)  ? "Info" : 
                (e.severity == lsp::DiagnosticSeverity::Hint)         ? "Hint" : 
                                                                        "Unknown";

            log::info("[{}] {}: {}", s, e.file, e.message);
        }
        log::info("--- Config Log ---");
        workspace_->projects_.print();
    }

    using FileDiags = std::unordered_map<std::filesystem::path, std::vector<lsp::Diagnostic>>;
    FileDiags fileDiags;

    auto makeDiag = [](
        const workspace::config::ConfigLog::Message& msg, 
        std::optional<std::filesystem::path> propagate_to_file,
        FileDiags& diags
    ) {
        lsp::Diagnostic diag;
        diag.message = msg.message;
        diag.severity = msg.severity;
        diag.range = lsp::Range{ lsp::Position{0,0}, lsp::Position{0,0} };
        
        auto file = msg.file;
        if(propagate_to_file) {
            if(propagate_to_file.value() == msg.file) return;
            file = propagate_to_file.value();
        }

        int display_count = 0;
        if(msg.context.has_value()) {
            auto literal = msg.context.value().literal;
            if(propagate_to_file) literal = "include";

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
    for (const auto& e : log.messages) {
        // Diagnosics for the file itself
        makeDiag(e, std::nullopt, fileDiags);

        // Propagate errors to the workspace config file
        if(e.severity == lsp::DiagnosticSeverity::Error) {
            makeDiag(e, workspace_->workspace_config_path, fileDiags);
        }
    }

    // Send diagnostics
    for(auto& [file, diags] : fileDiags) {
        message_handler_.sendNotification<notif::TextDocument_PublishDiagnostics>(
            notif::TextDocument_PublishDiagnostics::Params {
                .uri = lsp::FileUri::fromPath(file.string()),
                .diagnostics = diags
            }
        );
    }
}

// Server Events ----------------------------------------------------------------------

struct InitOptions {
    std::filesystem::path workspace_root;
    std::filesystem::path workspace_config_path;
    std::filesystem::path global_config_path;
};

static inline InitOptions parse_initialize_options(const reqst::Initialize::Params& params, Server& log) {
    InitOptions data;

    if (params.rootUri.isNull()) {
        log.send_message("No root URI provided in initialize request", lsp::MessageType::Error);
        return data;
    }

    data.workspace_root = std::string(params.rootUri.value().path());
    
    if (auto init = params.initializationOptions; init.has_value() && init->isObject()) {
        const auto& obj = init->object();
        if (auto it = obj.find("workspaceConfig"); it != obj.end() && it->second.isString()) {
            data.workspace_config_path = it->second.string();
        }
        if (auto it = obj.find("globalConfig"); it != obj.end() && it->second.isString()) {
            data.global_config_path = it->second.string();
        }
    } else {
        log.send_message("No workspace root provided in initialize request", lsp::MessageType::Error);
    }
    return data;
}

void Server::setup_events() {
    // Initilalize ----------------------------------------------------------------------
    message_handler_.add<reqst::Initialize>([this](reqst::Initialize::Params&& params) {
        log::info( "[LSP] <<< Initialize");
        
        InitOptions init_data = parse_initialize_options(params, *this);

        if(init_data.workspace_config_path.empty()) send_message("No local artic.json workspace config", lsp::MessageType::Warning);
        if(init_data.global_config_path.empty())    send_message("No global artic.json config", lsp::MessageType::Warning); 
        log::info("Workspace root: {}", init_data.workspace_root);
        log::info("Workspace config path: {}", init_data.workspace_config_path);
        log::info("Global config path: {}", init_data.global_config_path);
        workspace_ = std::make_unique<workspace::Workspace>(
            init_data.workspace_root, 
            init_data.workspace_config_path, 
            init_data.global_config_path
        );
        
        return reqst::Initialize::Result {
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

    message_handler_.add<notif::Initialized>([this](notif::Initialized::Params&&){
        log::info("[LSP] <<< Initialized");
        reload_workspace();
    });

    // Shutdown ----------------------------------------------------------------------
    message_handler_.add<reqst::Shutdown>([this]() {
        log::info("[LSP] <<< Shutdown");
        running_ = false;
        return reqst::Shutdown::Result {};
    });

    // Textdocument ----------------------------------------------------------------------
    message_handler_.add<notif::TextDocument_DidChange>([](notif::TextDocument_DidChange::Params&& params) {
        log::info("[LSP] <<< TextDocument DidChange");
    });
    message_handler_.add<notif::TextDocument_DidClose>([](notif::TextDocument_DidClose::Params&& params) {
        log::info("[LSP] <<< TextDocument DidClose");
    });
    message_handler_.add<notif::TextDocument_DidOpen>([this](notif::TextDocument_DidOpen::Params&& params) {
        log::info("[LSP] <<< TextDocument DidOpen");

        if(get_file_type(params.textDocument.uri.path()) == FileType::Source) {
            auto path = std::string(params.textDocument.uri.path());
            
            // skip compilation on open when it was already compiled
            // we need to do this as go to definition shortly opens the text document in vscode 
            // and we don't want to invalidate the definition while looking it up
            bool already_compiled = last_compile && last_compile->compiler->locator.data(path);
            if(!already_compiled)
                compile_file(path);
        }
    });
    message_handler_.add<notif::TextDocument_DidSave>([this](notif::TextDocument_DidSave::Params&& params) {
        log::info("[LSP] <<< TextDocument DidSave");
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

    message_handler_.add<notif::Workspace_DidChangeConfiguration>([this](notif::Workspace_DidChangeConfiguration::Params&& params) {
        log::info("[LSP] <<< Workspace DidChangeConfiguration");
        // Optionally, could inspect params.settings to override paths.
        reload_workspace();
    });
    message_handler_.add<notif::Workspace_DidChangeWatchedFiles>([this](notif::Workspace_DidChangeWatchedFiles::Params&& params) {
        log::info("[LSP] <<< Workspace DidChangeWatchedFiles");

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

    // notif::Workspace_DidChangeWorkspaceFolders
    // notif::Workspace_DidCreateFiles
    // notif::Workspace_DidDeleteFiles
    // notif::Workspace_DidRenameFiles

    // Other ----------------------------------------------------------------------

    // req::CallHierarchy_IncomingCalls
    // req::CallHierarchy_OutgoingCalls
    // req::Client_RegisterCapability
    // req::Client_UnregisterCapability
    // req::CodeAction_Resolve
    // req::CodeLens_Resolve
    // req::CompletionItem_Resolve
    // req::DocumentLink_Resolve
    // req::InlayHint_Resolve
    // req::TextDocument_CodeAction
    // req::TextDocument_CodeLens
    // req::TextDocument_ColorPresentation
    // req::TextDocument_Completion
    // req::TextDocument_Declaration
    
    message_handler_.add<reqst::TextDocument_Definition>([this](lsp::TextDocumentPositionParams&& pos) -> reqst::TextDocument_Definition::Result {
        log::info("[LSP] <<< TextDocument Definition {}:{}:{}", pos.textDocument.uri.path(), pos.position.line + 1, pos.position.character + 1);
        // TODO Currently unsupported:
        //  - paths resolve to first element (example: `my_mod::func()` -> goes to `my_mod` not `func`)
        //  - projection expressions         (example: `my_struct_var.field`)

        if (!last_compile || last_compile->stage < compiler::CompileResult::NameBinded) {
            return {};
        }

        auto& lsp_definition_map = last_compile->compiler->name_binder.lsp_definition_map;

        for (auto& [key, decl] : lsp_definition_map) {
            const auto& file = *key->elems.front().id.loc.file;
            if(pos.textDocument.uri.path() != file) continue;

            // currently only looks for last member in path
            for(auto& elem : key->elems){
                const auto& begin = elem.id.loc.begin;
                const auto& end   = elem.id.loc.end;

                auto line = pos.position.line + 1;
                auto col  = pos.position.character + 1;
                if(line != begin.row || col < begin.col) continue;
                if(line != end.row   || col > end.col) continue;
                // Found a matching key
                if(!decl->loc.file) return {}; // can happen for built-in decls like super::
                auto def_uri = lsp::FileUri::fromPath(*decl->loc.file);
                lsp::Location loc {
                    .uri = def_uri,
                    .range = lsp::Range {
                        .start = lsp::Position { static_cast<lsp::uint>(decl->loc.begin.row - 1), static_cast<lsp::uint>(decl->loc.begin.col - 1) },
                        .end   = lsp::Position { static_cast<lsp::uint>(decl->loc.end.row   - 1), static_cast<lsp::uint>(decl->loc.end.col   - 1) }
                    }
                };
                log::info("[LSP] >>> return TextDocument Definition {}:{}:{}", loc.uri.path(), loc.range.start.line + 1, loc.range.start.character + 1);
                return { loc };
            }
        }
        return {};
    });
    // req::TextDocument_Diagnostic
    // req::TextDocument_DocumentColor
    // req::TextDocument_DocumentHighlight
    // req::TextDocument_DocumentLink
    // req::TextDocument_DocumentSymbol
    // req::TextDocument_FoldingRange
    // req::TextDocument_Formatting
    // req::TextDocument_Hover
    // req::TextDocument_Implementation
    // req::TextDocument_InlayHint
    // req::TextDocument_InlineCompletion
    // req::TextDocument_InlineValue
    // req::TextDocument_LinkedEditingRange
    // req::TextDocument_Moniker
    // req::TextDocument_OnTypeFormatting
    // req::TextDocument_PrepareCallHierarchy
    // req::TextDocument_PrepareRename
    // req::TextDocument_PrepareTypeHierarchy
    // req::TextDocument_RangeFormatting
    // req::TextDocument_RangesFormatting
    // req::TextDocument_References
    // req::TextDocument_Rename
    // req::TextDocument_SelectionRange
    // req::TextDocument_SemanticTokens_Full
    // req::TextDocument_SemanticTokens_Full_Delta
    // req::TextDocument_SemanticTokens_Range
    // req::TextDocument_SignatureHelp
    // req::TextDocument_TypeDefinition
    // req::TextDocument_WillSaveWaitUntil
    // req::TypeHierarchy_Subtypes
    // req::TypeHierarchy_Supertypes
    // req::Window_ShowDocument
    // req::Window_ShowMessageRequest
    // req::Window_WorkDoneProgress_Create
    // req::Workspace_ApplyEdit
    // req::Workspace_CodeLens_Refresh
    // req::Workspace_Configuration
    // req::Workspace_Diagnostic
    // req::Workspace_Diagnostic_Refresh
    // req::Workspace_ExecuteCommand
    // req::Workspace_FoldingRange_Refresh
    // req::Workspace_InlayHint_Refresh
    // req::Workspace_InlineValue_Refresh
    // req::Workspace_SemanticTokens_Refresh
    // req::Workspace_Symbol
    // req::Workspace_WillCreateFiles
    // req::Workspace_WillDeleteFiles
    // req::Workspace_WillRenameFiles
    // req::Workspace_WorkspaceFolders
    // req::WorkspaceSymbol_Resolve
    // notif::CancelRequest
    // notif::LogTrace
    // notif::Progress
    // notif::SetTrace
    // notif::Exit
    // notif::Telemetry_Event
    // notif::TextDocument_PublishDiagnostics
    // notif::TextDocument_WillSave
    // notif::Window_LogMessage
    // notif::Window_ShowMessage
    // notif::Window_WorkDoneProgress_Cancel
}

} // namespace artic::ls