#ifndef ARTIC_BIND_H
#define ARTIC_BIND_H

#include <optional>
#include <unordered_map>
#include <string_view>
#include <variant>
#include <vector>
#include <algorithm>

#include "artic/symbol.h"
#include "artic/ast.h"
#include "artic/log.h"

namespace artic {

namespace ls {

/// Stores information related to LSP go-to-definiton & find-references
class NameMap {
public:
    using Decl = const ast::NamedDecl*;
    using Ref = std::variant<const ast::Identifier* /* used for ast::FieldExpr and ast::FieldPtrn */, 
                             const ast::Path*, const ast::Path::Elem*, 
                             const ast::ProjExpr*>;

    void insert(Decl decl, Ref ref);
    void insert(Decl def);

    const std::vector<Ref>& find_refs(Decl decl) const;
    Decl find_decl(Ref ref) const;

    Decl find_decl_at(const Loc& loc) const;
    std::optional<Ref> find_ref_at(const Loc& loc) const;

    const ast::Identifier& get_identifier(Ref ref) const;
private:
    struct Names {
        std::unordered_map<Ref, Decl> declaration_of;
        std::unordered_map<Decl, std::vector<Ref>> references_of;
    };
    std::unordered_map<std::string, Names> files;
};    

} // namespace ls


/// Binds identifiers to the nodes of the AST.
class NameBinder : public Logger {
public:
    NameBinder(Log& log, ls::NameMap* lsp = nullptr)
        : Logger(log)
        , name_map(lsp)
        , cur_fn(nullptr)
        , cur_loop(nullptr)
        , cur_mod(nullptr)
    {
        push_scope(true);
    }

    ~NameBinder() { pop_scope(); }

    ls::NameMap* name_map;

    /// Performs name binding on a whole program.
    /// Returns true on success, otherwise false.
    bool run(ast::ModDecl&);

    bool warn_on_shadowing = false;

    ast::FnExpr*   cur_fn;
    ast::LoopExpr* cur_loop;
    ast::ModDecl*  cur_mod;

    void bind_head(ast::Decl&);
    void bind(ast::Node&);

    void push_scope(bool top_level = false) { scopes_.emplace_back(top_level); }
    void pop_scope(bool warn_on_unused_identifiers = true);
    void insert_symbol(ast::NamedDecl&, const std::string&);
    void insert_symbol(ast::NamedDecl& decl) {
        insert_symbol(decl, decl.id.name);
    }

    void remove_symbol(const std::string& name) {
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            if (auto symbol = it->find(name)) {
                it->erase(name);
                return;
            }
        }
    }

    Symbol* find_symbol(const std::string& name) {
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++) {
            if (auto symbol = it->find(name)) {
                symbol->use_count++;
                return symbol;
            }
        }
        return nullptr;
    }

    Symbol* find_similar_symbol(const std::string& name) {
        Symbol* best = nullptr;
        auto min = levenshtein_threshold();
        for (auto it = scopes_.rbegin(); it != scopes_.rend(); it++)
            best = it->find_similar(name, min, levenshtein);
        return best;
    }

private:
    // Levenshtein distance is used to suggest similar identifiers to the user
    static constexpr size_t levenshtein_threshold() { return 3; }
    static size_t levenshtein(const std::string_view& a, const std::string_view& b, size_t max) {
        if (max == 0 || a.empty() || b.empty())
            return std::max(a.size(), b.size());
        auto d = a.front() != b.front() ? 1 : 0;
        auto d1 = levenshtein(a.substr(1), b, max - 1) + 1;
        auto d2 = levenshtein(a, b.substr(1), max - 1) + 1;
        auto d3 = levenshtein(a.substr(1), b.substr(1), max - d) + d;
        return std::min(d1, std::min(d2, d3));
    }

    std::vector<SymbolTable> scopes_;

    friend struct ast::ModDecl;
};

} // namespace artic

#endif // ARTIC_BIND_H
