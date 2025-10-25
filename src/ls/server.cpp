#include "artic/ls/server.h"

#include "artic/ls/config.h"
#include "artic/ls/crash.h"
#include "artic/ls/workspace.h"
#include "artic/log.h"
#include "artic/ast.h"
#include "artic/bind.h"
#include "artic/print.h"
#include "artic/types.h"
#include "lsp/error.h"

#include <limits>
#include <lsp/types.h>
#include <lsp/io/standardio.h>
#include <lsp/messages.h>

#include <fstream>
#include <string>
#include <string_view>
#include <cctype>
#include <sstream>
#include <algorithm>


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
    running_ = true;
    while (running_) {
        try {
            message_handler_.processIncomingMessages();
            // std::this_thread::sleep_for(std::chrono::milliseconds(10));
        } catch (const lsp::RequestError& e) {
            log::info("LSP Message processing error: {}", e.what());
        } catch (...) {
            log::info("LSP Server unknown fatal error");
            return 1;
        }
    }

    log::info("LSP Server shutdown complete");
    return 0;
}

void Server::send_message(const std::string& message, lsp::MessageType type) {
    message_handler_.sendNotification<notif::Window_ShowMessage>({ .type = type, .message = message });
}

Server::FileType Server::get_file_type(const std::filesystem::path& file) {
    return file.extension() == ".json" ? FileType::ConfigFile : FileType::SourceFile;
}

lsp::Location convert_loc(const Loc& loc){
    if (!loc.file) throw lsp::RequestError(lsp::Error::InternalError, "Cannot convert location with undefined file");
    return lsp::Location {
        .uri = lsp::FileUri::fromPath(*loc.file),
        .range = lsp::Range {
            .start = lsp::Position { static_cast<lsp::uint>(loc.begin.row - 1), static_cast<lsp::uint>(loc.begin.col - 1) },
            .end   = lsp::Position { static_cast<lsp::uint>(loc.end.row   - 1), static_cast<lsp::uint>(loc.end.col   - 1) }
        }
    };
}

Loc convert_loc(const lsp::TextDocumentIdentifier& file, const lsp::Position& pos) {
    return Loc(
        std::make_shared<std::string>(file.uri.path()),
        Loc::Pos { .row = static_cast<int>(pos.line + 1), .col = static_cast<int>(pos.character + 1) }
    );
}

Loc convert_loc(const lsp::TextDocumentIdentifier& file, const lsp::Range& pos) {
    return Loc(
        std::make_shared<std::string>(file.uri.path()),
        Loc::Pos { .row = static_cast<int>(pos.start.line + 1), .col = static_cast<int>(pos.start.character + 1) },
        Loc::Pos { .row = static_cast<int>(pos.end.line + 1),   .col = static_cast<int>(pos.end.character + 1) }
    );
}

// -----------------------------------------------------------------------------
//
//
// Initialization
//
//
// -----------------------------------------------------------------------------


struct InitOptions {
    std::filesystem::path workspace_root;
    std::filesystem::path workspace_config_path;
    std::filesystem::path global_config_path;
    bool restart_from_crash;
};

InitOptions parse_initialize_options(const reqst::Initialize::Params& params, Server& server) {
    if (params.rootUri.isNull())
        throw lsp::RequestError(lsp::Error::InvalidParams, "No root URI provided in initialize request");

    InitOptions data;
    data.workspace_root = std::string(params.rootUri.value().path());

    if (auto init = params.initializationOptions; init.has_value() && init->isObject()) {
        const auto& obj = init->object();
        if (auto it = obj.find("workspaceConfig"); it != obj.end() && it->second.isString())
            data.workspace_config_path = it->second.string();

        if (auto it = obj.find("globalConfig"); it != obj.end() && it->second.isString())
            data.global_config_path = it->second.string();
        
        if (auto it = obj.find("restartFromCrash"); it != obj.end() && it->second.isBoolean())
            data.restart_from_crash = it->second.boolean();
    } else {
        server.send_message("No initialization options provided in initialize request", lsp::MessageType::Error);
    }

    if(data.workspace_config_path.empty()) server.send_message("No local artic.json workspace config", lsp::MessageType::Warning);
    if(data.global_config_path.empty())    server.send_message("No global artic.json config", lsp::MessageType::Warning); 
    return data;
}

void Server::setup_events_initialization() {
    message_handler_.add<reqst::Initialize>([this](reqst::Initialize::Params&& params) -> reqst::Initialize::Result {
        Timer _("Initialize");
        log::info( "\n[LSP] <<< Initialize");
        
        InitOptions init_data = parse_initialize_options(params, *this);

        log::info("Workspace root: {}", init_data.workspace_root);
        log::info("Workspace config path: {}", init_data.workspace_config_path);
        log::info("Global config path: {}", init_data.global_config_path);

        safe_mode_ = init_data.restart_from_crash;
        workspace_ = std::make_unique<workspace::Workspace>(
            init_data.workspace_root, 
            init_data.workspace_config_path, 
            init_data.global_config_path
        );
        
        return reqst::Initialize::Result {
            .capabilities = lsp::ServerCapabilities{
                .textDocumentSync = lsp::TextDocumentSyncOptions{
                    .openClose = true,
                    .change    = lsp::TextDocumentSyncKind::Full,
                    .save      = lsp::SaveOptions{ .includeText = false },
                },
                .completionProvider = lsp::CompletionOptions{
                    .triggerCharacters = std::vector<std::string>{".", ":"}
                },
                .definitionProvider = true,
                .referencesProvider = true,
                .renameProvider = lsp::RenameOptions {
                    .prepareProvider = true
                },
                .semanticTokensProvider = lsp::SemanticTokensOptions{
                    .legend = lsp::SemanticTokensLegend{
                        .tokenTypes = {
                            "namespace", "type", "class", "enum", "interface", "struct", 
                            "typeParameter", "parameter", "variable", "property", "enumMember",
                            "event", "function", "method", "macro", "keyword",
                            "modifier", "comment", "string", "number", "regexp", "operator"
                        },
                        .tokenModifiers = {
                            "declaration", "definition", "readonly", "static", 
                            "deprecated", "abstract", "async", "modification", 
                            "documentation", "defaultLibrary"
                        }
                    },
                    .range = true,
                    .full = lsp::SemanticTokensOptionsFull{
                        .delta = false
                    }
                },
                .inlayHintProvider = lsp::InlayHintOptions {
                    .resolveProvider = false
                }
            },
            .serverInfo = lsp::InitializeResultServerInfo {
                .name    = "Artic Language Server",
                .version = "0.1.0"
            }
        };
    });

    message_handler_.add<notif::Initialized>([this](notif::Initialized::Params&&){
        log::info("\n[LSP] <<< Initialized");
        reload_workspace();
    });

    message_handler_.add<reqst::Shutdown>([this]() {
        log::info("\n[LSP] <<< Shutdown");
        running_ = false;
        return reqst::Shutdown::Result {};
    });
}


// -----------------------------------------------------------------------------
//
//
// Modifications (File changes)
//
//
// -----------------------------------------------------------------------------


void Server::setup_events_modifications() {

    // Textdocument ----------------------------------------------------------------------

    message_handler_.add<notif::TextDocument_DidClose>([](notif::TextDocument_DidClose::Params&& params) {
        log::info("\n[LSP] <<< TextDocument DidClose");
    });
    message_handler_.add<notif::TextDocument_DidOpen>([this](notif::TextDocument_DidOpen::Params&& params) {
        log::info("\n[LSP] <<< TextDocument DidOpen");

        if(get_file_type(params.textDocument.uri.path()) == FileType::SourceFile) {
            auto path = std::string(params.textDocument.uri.path());
            
            // skip compilation on open when it was already compiled
            // we need to do this as go to definition shortly opens the text document in vscode 
            // and we don't want to invalidate the definition while looking it up
            bool already_compiled = compile && compile->locator.data(path);
            if(!already_compiled)
                compile_file(path);
        }
    });
    message_handler_.add<notif::TextDocument_DidChange>([this](notif::TextDocument_DidChange::Params&& params) {
        log::info("");
        log::info("--------------------------------");
        log::info("[LSP] <<< TextDocument DidChange");
        std::filesystem::path file = params.textDocument.uri.path();
        if(get_file_type(file) == FileType::ConfigFile) {
            return;
        }
        // Clear the last compilation result to invalidate stale inlay hints
        // compile.reset();
        // workspace_->mark_file_dirty(file);

        auto& content = std::get<lsp::TextDocumentContentChangeEvent_Text>(params.contentChanges[0]).text;
        workspace_->set_file_content(file, std::move(content));
        compile_file(file);
    });

    message_handler_.add<notif::TextDocument_DidSave>([this](notif::TextDocument_DidSave::Params&& params) {
        log::info("\n[LSP] <<< TextDocument DidSave");
        // compile.reset();
        // auto file = params.textDocument.uri.path();
        // if(get_file_type(file) == FileType::ConfigFile) {
        //     reload_workspace();
        //     return;
        // }
        // compile_file(file);
        // (void)message_handler_.sendRequest<reqst::Workspace_SemanticTokens_Refresh>();
    });

    // Workspace ----------------------------------------------------------------------

    message_handler_.add<notif::Workspace_DidChangeConfiguration>([this](notif::Workspace_DidChangeConfiguration::Params&& params) {
        log::info("\n[LSP] <<< Workspace DidChangeConfiguration");
        // Optionally, could inspect params.settings to override paths.
        reload_workspace();
    });
    message_handler_.add<notif::Workspace_DidChangeWatchedFiles>([this](notif::Workspace_DidChangeWatchedFiles::Params&& params) {
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
}


// -----------------------------------------------------------------------------
//
//
// Semantic Tokens
//
//
// -----------------------------------------------------------------------------


struct SemanticToken {
    uint32_t line;
    uint32_t start; 
    uint32_t length;
    uint32_t type;
    uint32_t modifiers;
};

SemanticToken create_semantic_token(const Loc& loc, const ast::NamedDecl& decl, bool is_decl) {
    SemanticToken token {
        .line =   (uint32_t) loc.begin.row - 1,
        .start =  (uint32_t) loc.begin.col - 1,
        .length = (uint32_t) loc.end.col - loc.begin.col,
        .type = 0,
        .modifiers = 0,
    };
    using ty = lsp::SemanticTokenTypes;
    using md = lsp::SemanticTokenModifiers;

    auto flag = [](md mod) -> uint32_t  {
        uint32_t val = static_cast<uint32_t>(mod);
        return 1u << (val);
    };

    if (auto t = decl.isa<ast::StaticDecl>()) {
        token.type = (uint32_t) ty::Variable;
        token.modifiers |= flag(md::Static);
        if(!t->is_mut) token.modifiers |= flag(md::Readonly);
    } 
    else if (auto t = decl.isa<ast::LetDecl>()) {
        if(auto p = t->ptrn->isa<ast::PtrnDecl>()){
            token.type = (uint32_t) ty::Variable;
            if(!p->is_mut) token.modifiers |= flag(md::Readonly);
        }
    } 
    else if (auto t = decl.isa<ast::PtrnDecl>()) {
        token.type = (uint32_t) ty::Parameter;
        if(!t->is_mut) token.modifiers |= flag(md::Readonly);
    } 
    else if (decl.isa<ast::TypeParam>())  token.type = (uint32_t) ty::Type;
    else if (decl.isa<ast::FnDecl>())     token.type = (uint32_t) ty::Function;
    else if (decl.isa<ast::RecordDecl>()) token.type = (uint32_t) ty::Struct;
    else if (decl.isa<ast::EnumDecl>())   token.type = (uint32_t) ty::Enum;
    else if (decl.isa<ast::TypeDecl>())   token.type = (uint32_t) ty::Type;
    else if (decl.isa<ast::FieldDecl>())  token.type = (uint32_t) ty::Property;
    else if (decl.isa<ast::ModDecl>())    token.type = (uint32_t) ty::Namespace;
    else if (decl.isa<ast::UseDecl>())    token.type = (uint32_t) ty::Namespace;

    if(is_decl){
        token.modifiers |= flag(md::Definition);
        token.modifiers |= flag(md::Declaration);
    }
    
    if(decl.type) {
        if(auto fn = decl.type->isa<FnType>()){
            token.type = (uint32_t) ty::Function;
            if(fn->codom->isa<NoRetType>())
                token.type = (uint32_t) ty::Keyword; // continuation
        }
    }
    return token;
}

// Collect semantic tokens from the NameMap by iterating over declarations and references
lsp::SemanticTokens collect(
    const ls::NameMap& name_map, 
    const std::string& file, 
    int start_row = 0, 
    int end_row = std::numeric_limits<int>::max()
) {
    std::vector<SemanticToken> tokens;
    // Check if we have entries for this file
    if (!name_map.files.contains(file)) return {};
    
    auto& names = name_map.files.at(file);
    
    // Collect tokens from references (this is where we want semantic highlighting)
    for (const auto& [ref, decl] : names.declaration_of) {
        auto& loc = name_map.get_identifier(ref).loc;
        if(loc.begin.row >= start_row && loc.end.row <= end_row)
            tokens.push_back(create_semantic_token(loc, *decl, false));
    }
    
    // Collect tokens from declarations
    for (const auto& [decl, refs] : names.references_of) {
        auto& loc = decl->id.loc;
        if(loc.begin.row >= start_row && loc.end.row <= end_row)
            tokens.push_back(create_semantic_token(loc, *decl, true));
    }

    std::sort(tokens.begin(), tokens.end(), [](const SemanticToken& a, const SemanticToken& b) {
        if (a.line != b.line) return a.line < b.line;
        return a.start < b.start;
    });

    // Encode
    std::vector<uint32_t> data;
    data.reserve(tokens.size() * sizeof(SemanticToken) / sizeof(uint32_t));
    uint32_t prev_line = 0;
    uint32_t prev_start = 0;
    
    for (const auto& token : tokens) {
        // Delta-encode the tokens as required by LSP spec
        uint32_t delta_line = token.line - prev_line;
        uint32_t delta_start = (delta_line == 0) ? token.start - prev_start : token.start;
        
        data.push_back(delta_line);
        data.push_back(delta_start);
        data.push_back(token.length);
        data.push_back(token.type);
        data.push_back(token.modifiers);
        
        prev_line = token.line;
        prev_start = token.start;
    }
    
    return lsp::SemanticTokens{
        .data = data
    };
}

void Server::setup_events_tokens() {
    // Semantic Tokens ----------------------------------------------------------------------
    message_handler_.add<reqst::TextDocument_SemanticTokens_Full>([this](lsp::SemanticTokensParams&& params) -> reqst::TextDocument_SemanticTokens_Full::Result {
        Timer _("TextDocument_SemanticTokens_Full");
        std::string file(params.textDocument.uri.path());
        log::info("\n[LSP] <<< TextDocument SemanticTokens_Full {}", file);
        
        // semantic tokens are not allowed to trigger recompile as this is called right after document changed
        bool already_compiled = compile && compile->locator.data(file);
        if(!already_compiled) return nullptr;
        // ensure_compile(file);
        auto tokens = collect(compile->name_map, std::string(params.textDocument.uri.path()));
        
        log::info("[LSP] >>> Returning {} semantic tokens", tokens.data.size());
        return tokens;
    });

    message_handler_.add<reqst::TextDocument_SemanticTokens_Range>([this](lsp::SemanticTokensRangeParams&& params) -> reqst::TextDocument_SemanticTokens_Range::Result {
        Timer _("TextDocument_SemanticTokens_Range");
        std::string file(params.textDocument.uri.path());
        log::info("\n[LSP] <<< TextDocument SemanticTokens_Range {}:{}:{} to {}:{}", 
                 file,
                 params.range.start.line + 1, params.range.start.character + 1,
                 params.range.end.line + 1, params.range.end.character + 1);
        // semantic tokens are not allowed to trigger recompile as this is called right after document changed
        bool already_compiled = compile && compile->locator.data(file);
        if(!already_compiled) return nullptr;
        // ensure_compile(file);
        auto tokens = collect(
            compile->name_map, std::string(params.textDocument.uri.path()), 
            params.range.start.line + 1, 
            params.range.end.line + 1);
        
        log::info("[LSP] >>> Returning {} semantic tokens", tokens.data.size());
        return tokens;
    });
}


// -----------------------------------------------------------------------------
//
//
// Definitions
//
//
// -----------------------------------------------------------------------------

struct IndentifierOccurences{
    std::string name;
    std::vector<lsp::Location> all_occurences;

    // Additional info
    lsp::Location cursor_range;
    lsp::Location declaration_range;
};

std::optional<IndentifierOccurences> find_occurrences_of_identifier(Server& server, const Loc& cursor, bool include_declaration) {
    server.ensure_compile(*cursor.file);
    auto& name_map = server.compile->name_map;

    Loc cursor_range;
    const ast::NamedDecl* target_decl = name_map.find_decl_at(cursor);
    if(target_decl) {
        cursor_range = target_decl->id.loc;
        log::info("found declaration at cursor '{}'", target_decl->id.name);
    } else {
        if(auto ref = name_map.find_ref_at(cursor)) {
            auto id = name_map.get_identifier(*ref);
            cursor_range = id.loc;
            target_decl = name_map.find_decl(*ref);
            log::info("found reference at cursor '{}'", target_decl->id.name);
        }
    }
    // No symbol at cursor position
    if(!target_decl) return std::nullopt;

    std::vector<lsp::Location> locations;

    // Include the declaration itself if requested
    if (include_declaration) {
        locations.push_back(convert_loc(target_decl->id.loc));
    }

    // Find all references to this declaration
    for (auto ref : name_map.find_refs(target_decl)) {
        locations.push_back(convert_loc(name_map.get_identifier(ref).loc));
    }

    return IndentifierOccurences {
        .name = target_decl->id.name,
        .all_occurences = std::move(locations),
        .cursor_range = convert_loc(cursor_range),
        .declaration_range = convert_loc(target_decl->id.loc),
    };
}

void Server::setup_events_definitions() {
    message_handler_.add<reqst::TextDocument_Definition>([this](lsp::TextDocumentPositionParams&& pos) -> reqst::TextDocument_Definition::Result {
        Timer _("TextDocument_Definition");
        log::info("\n[LSP] <<< TextDocument Definition {}:{}:{}", pos.textDocument.uri.path(), pos.position.line + 1, pos.position.character + 1);

        auto cursor = convert_loc(pos.textDocument, pos.position);

        ensure_compile(pos.textDocument.uri.path());
        auto& name_map = compile->name_map;
        
        // When on a reference try find declaration
        if(auto ref = name_map.find_ref_at(cursor)) {
            if(auto def = name_map.find_decl(*ref)) {
                auto loc = convert_loc(def->id.loc);
                log::info("[LSP] >>> return TextDocument Definition {}:{}:{}", loc.uri.path(), loc.range.start.line + 1, loc.range.start.character + 1);
                return { loc };
            }
            return nullptr;
        }
        // When on a declaration try find references
        if(auto occurences = find_occurrences_of_identifier(*this, cursor, false)){
            log::info("[LSP] >>> Found {} occurrences of identifier", occurences->all_occurences.size());
            if(occurences->all_occurences.empty()) return { occurences->declaration_range };
            return occurences->all_occurences;
        }

        return nullptr;
    });

    message_handler_.add<reqst::TextDocument_References>([this](lsp::ReferenceParams&& params) -> reqst::TextDocument_References::Result {
        Timer _("TextDocument_References");
        log::info("\n[LSP] <<< TextDocument References {}:{}:{}", params.textDocument.uri.path(), params.position.line + 1, params.position.character + 1);

        auto cursor = convert_loc(params.textDocument, params.position);
        auto occurences = find_occurrences_of_identifier(*this, cursor, true);
        if(!occurences) return {};
        log::info("[LSP] >>> Found {} occurrences of identifier", occurences->all_occurences.size());
        return occurences->all_occurences;
    });

    message_handler_.add<reqst::TextDocument_PrepareRename>([this](lsp::TextDocumentPositionParams&& params) -> reqst::TextDocument_PrepareRename::Result {
        Timer _("TextDocument_PrepareRename");
        log::info("\n[LSP] <<< TextDocument PrepareRename {}:{}:{}", 
                params.textDocument.uri.path(), params.position.line + 1, params.position.character + 1);

        auto cursor = convert_loc(params.textDocument, params.position);
        auto occurences = find_occurrences_of_identifier(*this, cursor, true);
        if(!occurences) {
            log::info("[LSP] >>> PrepareRename found no symbol at cursor");
            return nullptr;
        }

        // Success: return the range of the symbol to be renamed
        log::info("[LSP] >>> PrepareRename successful for symbol '{}'", occurences->name);
        auto res = lsp::PrepareRenameResult_Range_Placeholder {
            .range = occurences->cursor_range.range,
            .placeholder = occurences->name
        };
        return lsp::PrepareRenameResult(res);
    });

    message_handler_.add<reqst::TextDocument_Rename>([this](lsp::RenameParams&& params) -> reqst::TextDocument_Rename::Result {
        Timer _("TextDocument_Rename");
        log::info("\n[LSP] <<< TextDocument Rename {}:{}:{} -> '{}'", 
                 params.textDocument.uri.path(), params.position.line + 1, params.position.character + 1, params.newName);

        auto cursor = convert_loc(params.textDocument, params.position);
        auto occurences = find_occurrences_of_identifier(*this, cursor, true);
        if(!occurences) {
            log::info("[LSP] >>> Rename found no symbol at cursor");
            return nullptr;
        }

        // Convert to LSP WorkspaceEdit format
        lsp::WorkspaceEdit workspace_edit;
        auto& changes = workspace_edit.changes.emplace();
        size_t total_edits = 0;
        for (auto& loc : occurences->all_occurences) {
            changes[loc.uri].emplace_back(
                lsp::TextEdit {
                    .range = loc.range,
                    .newText = params.newName
                }
            );
            ++total_edits;
        }

        log::info("[LSP] >>> Rename operation will edit {} files with {} total edits", workspace_edit.changes->size(), total_edits);

        return workspace_edit;
    });
}


// -----------------------------------------------------------------------------
//
//
// Completion
//
//
// -----------------------------------------------------------------------------


// Completion Helper Functions
std::string get_completion_detail(const ast::NamedDecl* decl) {
    if (auto fn_decl = decl->isa<ast::FnDecl>()) {
        return "function";
    } else if (auto static_decl = decl->isa<ast::StaticDecl>()) {
        return static_decl->is_mut ? "let mut" : "let";
    } else if (auto ptrn_decl = decl->isa<ast::PtrnDecl>()) {
        return ptrn_decl->is_mut ? "parameter mut" : "parameter";
    } else if (auto struct_decl = decl->isa<ast::StructDecl>()) {
        return "struct";
    } else if (auto enum_decl = decl->isa<ast::EnumDecl>()) {
        return "enum";
    } else if (auto type_decl = decl->isa<ast::TypeDecl>()) {
        return "type";
    } else if (auto field_decl = decl->isa<ast::FieldDecl>()) {
        return "field";
    } else if (auto mod_decl = decl->isa<ast::ModDecl>()) {
        return "module";
    }
    return "declaration";
}

lsp::CompletionItemKind get_completion_kind(const ast::NamedDecl* decl) {
    if (decl->isa<ast::FnDecl>()) return lsp::CompletionItemKind::Function;
    if (decl->isa<ast::StaticDecl>()) return lsp::CompletionItemKind::Variable;
    if (decl->isa<ast::PtrnDecl>()) return lsp::CompletionItemKind::Variable;
    if (decl->isa<ast::StructDecl>()) return lsp::CompletionItemKind::Struct;
    if (decl->isa<ast::EnumDecl>()) return lsp::CompletionItemKind::Enum;
    if (decl->isa<ast::TypeDecl>()) return lsp::CompletionItemKind::TypeParameter;
    if (decl->isa<ast::FieldDecl>()) return lsp::CompletionItemKind::Field;
    if (decl->isa<ast::ModDecl>()) return lsp::CompletionItemKind::Module;
    return lsp::CompletionItemKind::Text;
}

bool same_file(const Loc& a, const Loc& b) { return a.file && b.file && *a.file == *b.file; }
bool overlaps(const Loc& a, const Loc& b) { return a.end > /* important > */ b.begin && a.begin <= b.end; }

lsp::CompletionItem completion_item(const ast::FnDecl* fn) {
    lsp::CompletionItem item;
    item.insertTextFormat = lsp::InsertTextFormat::Snippet;
    std::stringbuf lb; 
    std::ostream str0(&lb);
    log::Output label(str0, false);
    Printer l(label);

    label << fn->id.name;

    if (fn->type_params) fn->type_params->print(l);
    if (auto* param = fn->fn->param.get()) {
        if (param->is_tuple()) {
            param->print(l);
        } else {
            l << '(';
            param->print(l);
            l << ')';
        }
    }
    
    item.label = lb.str();
    lb.str("");

    if(const auto* type = fn->type) {
        if(const auto* forall = fn->type->isa<ForallType>()) type = forall->body;
        if(type) if(const auto* f = fn->type->isa<FnType>()) {
            f->codom->print(l);
            item.detail = lb.str();
        }
    }
    if (!item.detail && fn->fn->ret_type) {
        fn->fn->ret_type->print(l);
        item.detail = lb.str();
    }

    std::stringbuf pt; 
    std::ostream str1(&pt);
    log::Output ptrn(str1, false);
    Printer p(ptrn);
    int arg = 1;
    ptrn << fn->id.name;
    if(fn->type_params && !fn->type_params->params.empty()) {
        ptrn << "[";
        for(int i = 0; i < fn->type_params->params.size(); i++) {
            if(i > 0) ptrn << ", ";
            ptrn << "${" << arg++ << ":" ;
            fn->type_params->params[i]->print(p);
            ptrn << "}";
        }
        ptrn << "]";
    }
    if(fn->fn->param){
        ptrn << "(";
        if(fn->fn->param->is_tuple()){
            auto tuple = fn->fn->param->isa<ast::TuplePtrn>();
            for(int i = 0; i < tuple->args.size(); i++) {
                if(i > 0) ptrn << ", ";
                ptrn << "${" << arg++ << ":" ;
                tuple->args[i]->print(p);
                ptrn << "}";
            }
        } else {
            ptrn << "${" << arg++ << ":" ;
            fn->fn->param->print(p);
            ptrn << "}";
        }
        ptrn << ")";
    }
    ptrn << "$0";
    item.insertText = pt.str();
    return item;
}

std::optional<lsp::CompletionItem> completion_item(const ast::NamedDecl& decl) {
    if(decl.id.name.empty()) return std::nullopt;
    if(decl.id.name.starts_with('_')) return std::nullopt;

    if (auto fn = decl.isa<ast::FnDecl>()) return completion_item(fn);

    lsp::CompletionItem item;

    // item.detail = get_completion_detail(&decl);
    item.kind = get_completion_kind(&decl);

    if(decl.type) {
        if (auto fn = decl.type->isa<FnType>()) {
            item.kind = lsp::CompletionItemKind::Function;

            std::stringbuf lb; 
            std::ostream str0(&lb);
            log::Output label(str0, false);
            Printer l(label);
            label << decl.id.name;
            if (fn->dom && fn->dom->isa<TupleType>()) {
                fn->dom->print(l);
            } else {
                l << '(';
                fn->dom->print(l);
                l << ')';
            }
            item.label = lb.str();
            if(fn->codom) {
                lb.str("");
                fn->codom->print(l);
                item.detail = lb.str();
            }

            std::stringbuf pt; 
            std::ostream str1(&pt);
            log::Output ptrn(str1, false);
            Printer p(ptrn);
            int arg = 1;
            ptrn << decl.id.name << "(";
            
            if(fn->dom) {
                if(const auto* tuple = fn->dom->isa<TupleType>()) {
                    for(int i = 0; i < tuple->args.size(); i++) {
                        if(i > 0) ptrn << ", ";
                        ptrn << "${" << arg++ << ":" ;
                        tuple->args[i]->print(p);
                        ptrn << "}";
                    }
                } else {
                    ptrn << "${" << arg++ << ":" ;
                    fn->dom->print(p);
                    ptrn << "}";
                }
            } 
            ptrn << ")";
            ptrn << "$0";
            item.insertText = pt.str();
        }
    }
    
    if(item.label.empty()){
        item.label = decl.id.name;
    }

    if (!item.detail && decl.type) {
        std::stringbuf lb; 
        std::ostream str0(&lb);
        log::Output label(str0, false);
        Printer l(label);
        decl.type->print(l);
        item.detail = lb.str();
    }
    return item;
}

void Server::setup_events_completion() {
    message_handler_.add<reqst::TextDocument_Completion>([this](lsp::CompletionParams&& params) -> reqst::TextDocument_Completion::Result {
        log::info("[LSP] <<< TextDocument Completion {}:{}:{}", 
                 params.textDocument.uri.path(), 
                 params.position.line + 1, 
                 params.position.character + 1);

        ensure_compile(params.textDocument.uri.path());
        if (!compile || !compile->program) {
            log::info("[LSP] >>> No compilation available for completion");
            return nullptr;
        }
        // params.position.character--;
        Loc cursor = convert_loc(params.textDocument, params.position);
        // const ast::ProjExpr* proj_expr = nullptr;
        // const ast::PathExpr* path_expr = nullptr;
        const ast::ModDecl* current_module = compile->program.get();
        // const ast::FnDecl* current_function = nullptr;
        std::vector<const ast::Node*> local_scopes;
        const ast::Node* outer_node = nullptr;
        const ast::Node* inner_node = nullptr;
        bool only_show_types = false;
        bool show_prim_types = true;
        bool inside_block_expr = false;
        bool top_level = false;

        std::vector<lsp::CompletionItem> items;

        ast::Node::TraverseFn traverse([&](const ast::Node& node) -> bool {
            if(!node.loc.file) return true; // super module
            if(!same_file(cursor, node.loc)) return false;
            // log::info("test node at {} vs {}", node.loc, loc);
            if(!overlaps(cursor, node.loc)) {
                return false;
            } else if(!outer_node) {
                outer_node = &node;
            }
            if(!only_show_types && (node.isa<ast::TypedExpr>() || node.isa<ast::TypedPtrn>() || node.isa<ast::TypeApp>())){
                only_show_types = true;
            } else if(const auto* mod = node.isa<ast::ModDecl>()){
                current_module = mod;
            } else if(const auto* fn = node.isa<ast::FnDecl>()){
                if(fn->fn->param) local_scopes.push_back(fn->fn->param.get());
                if(fn->type_params) local_scopes.push_back(fn->type_params.get());
            } else if(const auto* block = node.isa<ast::BlockExpr>()){
                local_scopes.push_back(block);
                inside_block_expr = true;
                top_level = false;
            } else if(const auto* error = node.isa<ast::ErrorDecl>(); error && error->is_top_level) {
                top_level = true;
            }
            inner_node = &node;

            log::info("Node at {}", node.loc);
            return true;
        });
        
        traverse(compile->program);

        log::Output out(std::clog, false);
        Printer p(out);
        p.print_additional_node_info = true;
        // if(outer_node) {
        //     log::info("\n-- Current Module");
        //     current_module->print(p);
        // }

        // if(inner_node) {
        //     log::info("\n-- Inner Node");
        //     inner_node->print(p);
        // }


        if(inner_node) {
            // Projection expression: a.b
            if(const auto* proj_expr = inner_node->isa<ast::ProjExpr>()) {
                // log::info("inside proj expr completion");
                proj_expr->dump();
                const Type* type = nullptr;
                if(proj_expr->type && !proj_expr->type->isa<TypeError>()) {
                    // log::info("using proj expr type");
                    type = proj_expr->type;
                }
                else if(proj_expr->expr->type && !proj_expr->expr->type->isa<TypeError>()){
                    // log::info("using proj expr expr type");
                    type = proj_expr->expr->type;
                }
                if(type){
                    if(auto addr = type->isa<AddrType>(); addr && addr->pointee) { type = addr->pointee; }
                    // type->dump();
                    if(auto struct_type = type->isa<StructType>()){
                        for (auto& field : struct_type->decl.fields) {
                            if(auto item = completion_item(*field)) items.push_back(std::move(*item));
                        }
                    }
                }
                current_module = nullptr; // no top level declarations
                show_prim_types = false;
            } 
            // Path expression a::b
            else if(const auto* path = inner_node->isa<ast::Path>(); path && path->elems.size() > 1) {
                const ast::Path::Elem* path_elem = &path->elems.front();
                for (const auto& elem: path->elems){
                    if(cursor.end > elem.loc.end) {
                        path_elem = &elem;
                    }
                }

                // log::info("path expression complete");
                // path->dump();
                // log::info("after element: {}", path_elem->id.name);
                if(path_elem->type) {
                    if(const auto* mod = path_elem->type->isa<ModType>()) {
                        current_module = &mod->decl;
                    }
                }
                show_prim_types = false;
                inside_block_expr = false; // dont show local vars
            }
        }
        
        log::info("show only types {}", only_show_types);
        log::info("show prim types {}", show_prim_types);
        log::info("has module: {}", (bool)current_module);
        
        if(current_module && !top_level){          
            // decls inside current module
            for (const auto& decl : current_module->decls) {
                if (const auto* named_decl = decl->isa<ast::NamedDecl>(); named_decl && 
                    (!only_show_types || decl->isa<ast::CtorDecl>() || decl->isa<ast::ModDecl>() || decl->isa<ast::TypeParam>() || decl->isa<ast::TypeDecl>() || decl->isa<ast::UseDecl>())
                ) {
                    if(auto item = completion_item(*named_decl)) items.push_back(std::move(*item));
                }
            }

            if (inside_block_expr){
                ast::Node::TraverseFn collect_local_decls([&](const ast::Node& node) -> bool {
                    if(collect_local_decls.depth > 0 && node.isa<ast::BlockExpr>()) {
                        return false; // do not go into nested blocks
                    }
                    if(const auto* named_decl = node.isa<ast::NamedDecl>(); named_decl) {
                        if(auto item = completion_item(*named_decl)) items.push_back(std::move(*item));
                    }
                    return true;
                });
                for (const auto* scope : local_scopes) {
                    collect_local_decls(*scope);
                }
            }
        }

        // Local snippets
        if(current_module && inside_block_expr) {
            items.push_back(lsp::CompletionItem {
                .label = "for",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "For Loop",
                .insertText = "for ${1:i} in ${2:range} {\n\t$0\n}",
            });

            items.push_back(lsp::CompletionItem {
                .label = "forrange",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Range For Loop",
                .insertText = "for ${1:i} in range(${2:0}, ${3:count}) {\n\t$0\n}",
            });

            items.push_back(lsp::CompletionItem {
                .label = "if",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "If Statement",
                .insertText = "if ${1:condition} {\n\t$0\n}",
            });

            items.push_back(lsp::CompletionItem {
                .label = "else",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Else Statement",
                .insertText = "else {\n\t$0\n}",
            });

            items.push_back(lsp::CompletionItem {
                .label = "match",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Match Expression",
                .insertText = "match ${1:expression} {\n\t${2:pattern} => ${3:result},\n\t${0}\n}",
            });

            items.push_back(lsp::CompletionItem {
                .label = "let",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Let Binding",
                .insertText = "let ${1:variable} = ${2:value};$0",
            });

            items.push_back(lsp::CompletionItem {
                .label = "return",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Return Statement",
                .insertText = "return($1)$0",
            });

            items.push_back(lsp::CompletionItem {
                .label = "continue",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Continue Statement",
                .insertText = "continue()",
            });

            items.push_back(lsp::CompletionItem {
                .label = "break",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Break Statement",
                .insertText = "break()",
            });

            items.push_back(lsp::CompletionItem {
                .label = "asm",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Assembly Block",
                .insertText = "asm(\"$1\"$2);$0",
            });
        }
        
        // Top level snippets
        if(current_module && top_level) {
            items.push_back(lsp::CompletionItem {
                .label = "fn",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Function Declaration",
                .insertText = "fn @${1:function}($2) -> ${3:ret_type} {\n\t$0\n}",
            });

            items.push_back(lsp::CompletionItem {
                .label = "struct",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Struct Declaration",
                .insertText = "struct ${1:StructName} {\n\t${0}\n}",
            });

            items.push_back(lsp::CompletionItem {
                .label = "record",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Record Declaration",
                .insertText = "struct ${1:RecordName}($2);$0",
            });

            items.push_back(lsp::CompletionItem {
                .label = "mod",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Module Declaration",
                .insertText = "mod ${1:module_name} {\n\t${0}\n}",
            });

            items.push_back(lsp::CompletionItem {
                .label = "enum",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Enum Declaration",
                .insertText = "enum ${1:EnumName} {\n\t${0}\n}",
            });

            items.push_back(lsp::CompletionItem {
                .label = "static",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Static Declaration",
                .insertText = "static ${1:variable} = ${2:value};$0",
            });

            items.push_back(lsp::CompletionItem {
                .label = "type",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Type Alias Declaration",
                .insertText = "type ${1:TypeName} = ${2:UnderlyingType};$0",
            });

            items.push_back(lsp::CompletionItem {
                .label = "use",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "Use Declaration",
                .insertText = "use ${1:module_name} as ${2:alias_name};$0",
            });
        }
        
        if(show_prim_types && !top_level) {
            auto show_prim_type = [&](std::string_view prim){
                lsp::CompletionItem item;
                item.kind = lsp::CompletionItemKind::Keyword;
                item.label = prim;
                items.push_back(std::move(item));
            };
            auto& types = compile->type_table;
            show_prim_type("bool");
            show_prim_type("i8");
            show_prim_type("i16");
            show_prim_type("i32");
            show_prim_type("i64");
            show_prim_type("u8");
            show_prim_type("u16");
            show_prim_type("u32");
            show_prim_type("u64");
            show_prim_type("f16");
            show_prim_type("f32");
            show_prim_type("f64");
            show_prim_type("simd");
            show_prim_type("mut");
            show_prim_type("super");

            
            items.push_back(lsp::CompletionItem {
                .label = "simd[...]",
                .kind = lsp::CompletionItemKind::Keyword,
                .insertText = "simd[${1:expr}]$0",
            });

            items.push_back(lsp::CompletionItem {
                .label = "addrspace(...)",
                .kind = lsp::CompletionItemKind::Keyword,
                .insertText = "addrspace(${1:1})$0",
            });

            items.push_back(lsp::CompletionItem {
                .label = "void",
                .kind = lsp::CompletionItemKind::Keyword,
                .detail = "()",
                .insertText = "()",
            });
        }

        std::reverse(items.begin(), items.end());

        lsp::CompletionList result{
            .isIncomplete = false,
            .items = std::move(items),
            .itemDefaults = lsp::CompletionListItemDefaults{ .insertTextFormat = lsp::InsertTextFormat::Snippet },
        };

        log::info("[LSP] >>> Returning {} completion items", result.items.size());
        return result;
    });
}


// -----------------------------------------------------------------------------
//
//
// Server Compilation / Diagnostics
//
//
// -----------------------------------------------------------------------------

void Server::compile_file(const std::filesystem::path& file) {
    Timer _("Compile Files");

    std::vector<const workspace::File*> files;
    compile.emplace();
    compile->active_file = file;

    if(auto proj = workspace_->project_for_file(file)){
        // known project    
        log::info("Compiling file {} (project '{}')", file, proj.value()->name);

        files = proj.value()->collect_files();
    } else {
        // default project
        auto default_proj = workspace_->default_project();
        
        log::info("Compiling file {} (not in workspace -> using default project {})", file, default_proj->name);
        
        files = default_proj->collect_files();
        auto temp_file = std::make_unique<workspace::File>(file);
        temp_file->read();
        files.push_back(temp_file.get());

        // keep the temporary file alive for diagnostics
        compile->temporary_files.push_back(std::move(temp_file));
    }
    
    if (files.empty()) {
        log::info("no input files (compile_files)");
        return;
    }

    log::info("Compiling {} file(s)", files.size());

    if(safe_mode_) {
        compile->exclude_non_parsed_files = true;
        log::info("Using safe mode");
    }

    compile->compile_files(files);

    if(safe_mode_ && compile->parsed_all) {
        safe_mode_ = false;
        log::info("Successfully parsed all files, turning off safe mode");
    }

    const bool print_compile_log = false;
    if(print_compile_log) compile->log.print_summary();

    if(compile->log.errors == 0){
        log::info("Compile success");
    } else {
        log::info("Compile failed");
    }

    // log::Output out(std::clog, false);
    // Printer p(out);
    // p.print_additional_node_info = true;
    // for(auto& [decl, _]: compile->name_map.files[file].references_of) {
    //     if(decl->is_top_level)
    //         decl->print(p);
    // }

    auto convert_diagnostic = [](const Diagnostic& diag) -> lsp::Diagnostic {
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
    for (const auto& diag : compile->diagnostics) {
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

void Server::ensure_compile(std::string_view file_view) {
    std::string file(file_view);
    bool already_compiled = compile && compile->locator.data(file);
    if (!already_compiled) compile_file(file);
    if (!compile) throw lsp::RequestError(lsp::Error::InternalError, "Did not get a compilation result");
}


// -----------------------------------------------------------------------------
//
//
// Server Reload Workspace
//
//
// -----------------------------------------------------------------------------


using FileDiags = std::unordered_map<std::filesystem::path, std::vector<lsp::Diagnostic>>;

void make_config_diagnostic(const workspace::config::ConfigLog::Message& msg, 
    std::optional<std::filesystem::path> propagate_to_file,
    FileDiags& diags
) {
    auto find_in_file = [](std::filesystem::path const& file, std::string_view literal) -> std::vector<lsp::Range> {
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
    };
    lsp::Diagnostic diag;
    diag.message = msg.message;
    diag.severity = msg.severity;
    diag.range = lsp::Range{ lsp::Position{0,0}, lsp::Position{0,0} };
    
    auto file = msg.file;
    if(propagate_to_file) {
        if(propagate_to_file.value() == msg.file) return;
        file = propagate_to_file.value();
        diag.message = "[" + msg.file.string() + "] " + diag.message;
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

void Server::reload_workspace(const std::string& active_file) {
    Timer _("Reload Workspace");
    log::info("Reloading workspace configuration");
    workspace::config::ConfigLog log;
    workspace_->reload(log);
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

    FileDiags fileDiags;

    // create diagnostics
    for (const auto& e : log.messages) {
        // Diagnosics for the file itself
        make_config_diagnostic(e, std::nullopt, fileDiags);

        // Propagate errors to the workspace config file
        if(e.severity == lsp::DiagnosticSeverity::Error) {
            make_config_diagnostic(e, workspace_->workspace_config_path, fileDiags);
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

    if(compile){
        auto file = compile->active_file;
        compile_file(file);
    }
}


// -----------------------------------------------------------------------------
//
//
// Other
//
//
// -----------------------------------------------------------------------------

void Server::setup_events_other() {

    message_handler_.add<reqst::TextDocument_InlayHint>([this](reqst::TextDocument_InlayHint::Params&& params) -> reqst::TextDocument_InlayHint::Result {
        Timer _("TextDocument_InlayHint");
        std::string file(params.textDocument.uri.path());
        log::info("\n[LSP] <<< TextDocument InlayHint {}:{}:{} to {}:{}", 
            file, 
            params.range.start.line + 1, params.range.start.character + 1,
            params.range.end.line + 1, params.range.end.character + 1);

        // inlay hints are not allowed to trigger recompile as this is called right after document changed
        bool already_compiled = compile && compile->locator.data(file);
        if(!already_compiled) return nullptr;
        // ensure_compile(file);

        lsp::Array<lsp::InlayHint> hints;
        if(!compile->name_map.files.contains(file))
            return hints;

        // Convert TypeHint structs to LSP InlayHint objects
        for (const auto* hint : compile->name_map.files.at(file).with_type_hint) {
            auto& loc = hint->loc;
            auto* type = hint->type;
            // Check if the hint location is within the requested range
            if (!loc.file || *loc.file != file) {
                continue;
            }

            lsp::Position hint_pos{
                static_cast<lsp::uint>(loc.end.row - 1),
                static_cast<lsp::uint>(hint->loc.end.col - 1)
            };

            // Check if the hint position is within the requested range
            if (hint_pos.line < params.range.start.line || 
                hint_pos.line > params.range.end.line ||
                (hint_pos.line == params.range.start.line && hint_pos.character < params.range.start.character) ||
                (hint_pos.line == params.range.end.line && hint_pos.character > params.range.end.character)) {
                continue;
            }

            // Format the type name for display
            std::string type_name = "<unknown>";
            if (type) {
                std::ostringstream oss;
                log::Output output(oss, false);
                Printer printer(output);
                type->print(printer);
                type_name = oss.str();
            }
            
            lsp::InlayHint lsp_hint;
            lsp_hint.position = hint_pos;
            lsp_hint.label = ": " + type_name;
            lsp_hint.kind = lsp::InlayHintKindEnum(lsp::InlayHintKind::Type);
            lsp_hint.paddingLeft = false;
            lsp_hint.paddingRight = true;
            
            hints.push_back(lsp_hint);
        }

        log::info("[LSP] >>> Returning {} inlay hints", hints.size());
        return hints;
    });

    // notif::Workspace_DidChangeWorkspaceFolders
    // notif::Workspace_DidCreateFiles
    // notif::Workspace_DidDeleteFiles
    // notif::Workspace_DidRenameFiles

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