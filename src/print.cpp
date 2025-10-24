#include <algorithm>

#include "artic/print.h"
#include "artic/log.h"
#include "artic/ast.h"
#include "artic/types.h"

namespace artic {

template <typename L, typename S, typename F>
void print_list(Printer& p, const S& sep, const L& list, F f) {
    for (auto it = list.begin(); it != list.end(); ++it) {
        f(*it);
        if (std::next(it) != list.end()) p << sep;
    }
}

template <typename E>
void print_parens(Printer& p, const E& e) {
    if (e->is_tuple()) {
        e->print(p);
    } else {
        p << '(';
        e->print(p);
        p << ')';
    }
}

// AST nodes -----------------------------------------------------------------------

struct NodeScope {
    Printer& p;
    std::string name;
    NodeScope(Printer& p, std::string_view name) : p(p), name(name) {
        if(!p.print_additional_node_info) return;
        p << p.endl() << p.indent();

        p << "<" << name << ">";

        p << p.endl();
    }
    ~NodeScope() {
        if(!p.print_additional_node_info) return;
        p << p.unindent();
    }
};

namespace ast {

void Path::print(Printer& p) const {
    NodeScope _(p, "Path");
    print_list(p, "::", elems, [&] (auto& e) {
        if (e.is_super())
            p << log::keyword_style(e.id.name);
        else
            p << e.id.name;
        if (!e.args.empty()) {
            p << '[';
            print_list(p, ", ", e.args, [&] (auto& arg) {
                arg->print(p);
            });
            p << ']';
        }
    });
}

void Filter::print(Printer& p) const {
    NodeScope _(p, "Filter");
    p << '@';
    if (expr) {
        p << '(';
        expr->print(p);
        p << ") ";
    }
}

// Attributes ----------------------------------------------------------------------

void PathAttr::print(Printer& p) const {
    NodeScope _(p, "PathAttr");
    p << name << " = ";
    path.print(p);
}

void LiteralAttr::print(Printer& p) const {
    NodeScope _(p, "LiteralAttr");
    p << name << " = " << std::showpoint << log::literal_style(lit);
}

void NamedAttr::print(Printer& p) const {
    NodeScope _(p, "NamedAttr");
    p << name;
    if (!args.empty()) {
        p << '(';
        print_list(p, ", ", args, [&] (auto& a) {
            a->print(p);
        });
        p << ')';
    }
}

void AttrList::print(Printer& p) const {
    NodeScope _(p, "AttrList");
    p << "#[";
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ']' << p.endl();
}

// Statements ----------------------------------------------------------------------

void DeclStmt::print(Printer& p) const {
    NodeScope _(p, "DeclStmt");
    decl->print(p);
}

void ExprStmt::print(Printer& p) const {
    NodeScope _(p, "ExprStmt");
    expr->print(p);
}

void TypedExpr::print(Printer& p) const {
    NodeScope _(p, "TypedExpr");
    expr->print(p);
    p << ": ";
    type->print(p);
}

void PathExpr::print(Printer& p) const {
    NodeScope _(p, "PathExpr");
    path.print(p);
}

void LiteralExpr::print(Printer& p) const {
    NodeScope _(p, "LiteralExpr");
    p << std::showpoint << log::literal_style(lit);
}

void SummonExpr::print(Printer& p) const {
    NodeScope _(p, "SummonExpr");
    if (resolved) {
        resolved->print(p);
        return;
    }
    p << log::keyword_style("summon") << "[";
    if (type) type->print(p);
    else if (type_expr) type_expr->print(p);
    p << "]";
}

void FieldExpr::print(Printer& p) const {
    NodeScope _(p, "FieldExpr");
    p << id.name << " = ";
    expr->print(p);
}

void RecordExpr::print(Printer& p) const {
    NodeScope _(p, "RecordExpr");
    if (expr) {
        expr->print(p);
        p << " .";
    } else
        type->print(p);
    p << " {";
    if (!fields.empty()) {
        p << ' ';
        print_list(p, ", ", fields, [&] (auto& f) {
            f->print(p);
        });
        p << ' ';
    }
    p << "}";
}

void TupleExpr::print(Printer& p) const {
    NodeScope _(p, "TupleExpr");
    p << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ')';
}

void ArrayExpr::print(Printer& p) const {
    NodeScope _(p, "ArrayExpr");
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    print_list(p, ", ", elems, [&] (auto& a) {
        a->print(p);
    });
    p << ']';
}

void RepeatArrayExpr::print(Printer& p) const {
    NodeScope _(p, "RepeatArrayExpr");
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    elem->print(p);
    p << "; ";
    std::visit([&] (auto&& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, size_t>)
                p << arg;
            else if constexpr (std::is_same_v<T, ast::Path&>)
                arg->print(p);
    }, size);
    p << ']';
}

void FnExpr::print(Printer& p) const {
    NodeScope _(p, "FnExpr");
    if (filter)
        filter->print(p);
    p << '|';
    if (auto tuple = param->isa<TuplePtrn>()) {
        print_list(p, ", ", tuple->args, [&] (auto& a) {
            a->print(p);
        });
    } else {
        param->print(p);
    }
    p << "| ";
    if (ret_type) {
        p << "-> ";
        ret_type->print(p);
        p << ' ';
    }
    if(body) body->print(p);
}

void BlockExpr::print(Printer& p) const {
    NodeScope _(p, "BlockExpr");
    if (stmts.empty())
        p << "{}";
    else {
        p << '{' << p.indent();
        for (size_t i = 0, n = stmts.size(); i < n; i++) {
            auto& stmt = stmts[i];
            p << p.endl();
            stmt->print(p);
            if ((i != n - 1 && stmt->needs_semicolon()) || (i == n - 1 && last_semi))
                p << ';';
        }
        p << p.unindent() << p.endl() << "}";
    }
}

void CallExpr::print(Printer& p) const {
    NodeScope _(p, "CallExpr");
    if (callee->isa<FnExpr>())
        print_parens(p, callee);
    else
        callee->print(p);
    print_parens(p, arg);
}

void ProjExpr::print(Printer& p) const {
    NodeScope _(p, "ProjExpr");
    expr->print(p);
    p << '.';
    std::visit([&] (auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, Identifier>)
            p << arg.name;
        else if constexpr (std::is_same_v<T, size_t>)
            p << arg;
    }, field);
}

void IfExpr::print(Printer& p) const {
    NodeScope _(p, "IfExpr");
    p << log::keyword_style("if") << ' ';
    if (cond)
        cond->print(p);
    else {
        p << log::keyword_style("let") << ' ';
        ptrn->print(p);
        p << " = ";
        expr->print(p);
    }
    p << ' ';
    if_true->print(p);
    if (if_false) {
        p << ' ' << log::keyword_style("else") << ' ';
        if_false->print(p);
    }
}

void CaseExpr::print(Printer& p) const {
    NodeScope _(p, "CaseExpr");
    ptrn->print(p);
    p << " => ";
    expr->print(p);
}

void MatchExpr::print(Printer& p) const {
    NodeScope _(p, "MatchExpr");
    p << log::keyword_style("match") << ' ';
    arg->print(p);
    p << " {" << p.indent();
    for (size_t i = 0, n = cases.size(); i < n; i++) {
        p << p.endl();
        cases[i]->print(p);
        if (i != n - 1)
            p << ",";
    }
    p << p.unindent() << p.endl() << "}";
}

void WhileExpr::print(Printer& p) const {
    NodeScope _(p, "WhileExpr");
    p << log::keyword_style("while") << ' ';
    if (cond)
        cond->print(p);
    else {
        p << log::keyword_style("let") << ' ';
        ptrn->print(p);
        p << " = ";
        expr->print(p);
    }
    p << ' ';
    body->print(p);
}

void ForExpr::print(Printer& p) const {
    NodeScope _(p, "ForExpr");
    auto& iter = call->callee->as<ast::CallExpr>()->callee;
    auto lambda = call->callee->as<ast::CallExpr>()->arg->as<ast::FnExpr>();
    p << log::keyword_style("for") << ' ';
    lambda->param->print(p);
    p << ' ' << log::keyword_style("in") << ' ';
    iter->print(p);
    print_parens(p, call->arg);
    p << ' ';
    lambda->body->print(p);
}

void BreakExpr::print(Printer& p) const {
    NodeScope _(p, "BreakExpr");
    p << log::keyword_style("break");
}

void ContinueExpr::print(Printer& p) const {
    NodeScope _(p, "ContinueExpr");
    p << log::keyword_style("continue");
}

void ReturnExpr::print(Printer& p) const {
    NodeScope _(p, "ReturnExpr");
    p << log::keyword_style("return");
}

void UnaryExpr::print(Printer& p) const {
    NodeScope _(p, "UnaryExpr");
    if (is_prefix()) {
        p << tag_to_string(tag);
        if (tag == AddrOfMut)
            p << log::keyword_style("mut") << ' ';
    }
    if (arg->isa<PathExpr>() || arg->isa<LiteralExpr>())
        arg->print(p);
    else
        print_parens(p, arg);
    if (is_postfix()) p << tag_to_string(tag);
}

void BinaryExpr::print(Printer& p) const {
    NodeScope _(p, "BinaryExpr");
    auto prec = BinaryExpr::precedence(tag);
    auto print_op = [prec, &p] (const Ptr<Expr>& e, bool is_right) {
        bool needs_parens = e->isa<IfExpr>() || e->isa<MatchExpr>();
        if (auto binary_expr = e->isa<BinaryExpr>()) {
            needs_parens =
                binary_expr->precedence() > prec ||
                (is_right && binary_expr->precedence() == prec);
        }
        if (needs_parens)
            print_parens(p, e);
        else
            e->print(p);
    };
    print_op(left, false);
    p << " " << tag_to_string(tag) << " ";
    print_op(right, true);
}

void FilterExpr::print(Printer& p) const {
    NodeScope _(p, "FilterExpr");
    filter->print(p);
    expr->print(p);
}

void CastExpr::print(Printer& p) const {
    NodeScope _(p, "CastExpr");
    if (expr->isa<BinaryExpr>())
        print_parens(p, expr);
    else
        expr->print(p);
    p << ' ' << log::keyword_style("as") << ' ';
    type->print(p);
}

void ImplicitCastExpr::print(Printer& p) const {
    NodeScope _(p, "ImplicitCastExpr");
    if (p.show_implicit_casts)
        p << "/* implicit cast to '" << *type << "' ( */";
    expr->print(p);
    if (p.show_implicit_casts)
        p << "/* ) */";
}

void AsmExpr::print(Printer& p) const {
    NodeScope _(p, "AsmExpr");
    p << log::keyword_style("asm") << '(' << p.indent() << p.endl()
      << '\"' << src << '\"' << p.endl();
    auto print_constr = [&] (auto& constr) {
        p << '\"' << constr.name << "\"(";
        constr.expr->print(p);
        p << ')';
    };
    auto print_clob = [&] (auto& clob) { p << '\"' << clob << '\"'; };
    auto print_opt = print_clob;
    p << ": ";
    print_list(p, ", ", outs, print_constr);
    p << ": ";
    print_list(p, ", ", ins, print_constr);
    p << ": ";
    print_list(p, ", ", clobs, print_clob);
    p << ": ";
    print_list(p, ", ", opts, print_opt);
    p << ")" << p.unindent();
}

void ErrorExpr::print(Printer& p) const {
    NodeScope _(p, "ErrorExpr");
    p << log::error_style("<invalid expression>");
}

void TypedPtrn::print(Printer& p) const {
    NodeScope _(p, "TypedPtrn");
    if (ptrn) {
        ptrn->print(p);
        p << ": ";
    }
    type->print(p);
}

void IdPtrn::print(Printer& p) const {
    NodeScope _(p, "IdPtrn");
    decl->print(p);
    if (sub_ptrn) {
        p << ' ' << log::keyword_style("as") << ' ';
        sub_ptrn->print(p);
    }
}

void LiteralPtrn::print(Printer& p) const {
    NodeScope _(p, "LiteralPtrn");
    p << std::showpoint << log::literal_style(lit);
}

void ImplicitParamPtrn::print(Printer& p) const {
    NodeScope _(p, "ImplicitParamPtrn");
    p << log::keyword_style("implicit") << ' ';
    underlying->print(p);
}

void FieldPtrn::print(Printer& p) const {
    NodeScope _(p, "FieldPtrn");
    if (is_etc()) {
        p << "...";
    } else {
        p << id.name << " = ";
        ptrn->print(p);
    }
}

void RecordPtrn::print(Printer& p) const {
    NodeScope _(p, "RecordPtrn");
    path.print(p);
    p << " {";
    if (!fields.empty()) {
        p << ' ';
        print_list(p, ", ", fields, [&] (auto& field) {
            field->print(p);
        });
        p << ' ';
    }
    p << "}";
}

void CtorPtrn::print(Printer& p) const {
    NodeScope _(p, "CtorPtrn");
    path.print(p);
    if (arg) print_parens(p, arg);
}

void TuplePtrn::print(Printer& p) const {
    NodeScope _(p, "TuplePtrn");
    p << '(';
    print_list(p, ", ", args, [&] (auto& arg) {
        arg->print(p);
    });
    p << ')';
}

void ArrayPtrn::print(Printer& p) const {
    NodeScope _(p, "ArrayPtrn");
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    print_list(p, ", ", elems, [&] (auto& elem) {
        elem->print(p);
    });
    p << ']';
}

void ErrorPtrn::print(Printer& p) const {
    NodeScope _(p, "ErrorPtrn");
    p << log::error_style("<invalid pattern>");
}

void TypeParam::print(Printer& p) const {
    NodeScope _(p, "TypeParam");
    p << id.name;
}

void TypeParamList::print(Printer& p) const {
    NodeScope _(p, "TypeParamList");
    if (!params.empty()) {
        p << '[';
        print_list(p, ", ", params, [&] (auto& param) {
            param->print(p);
        });
        p << ']';
    }
}

void PtrnDecl::print(Printer& p) const {
    NodeScope _(p, "PtrnDecl");
    if (is_mut) p << log::keyword_style("mut") << ' ';
    p << id.name;
}

void LetDecl::print(Printer& p) const {
    NodeScope _(p, "LetDecl");
    if (attrs) attrs->print(p);
    p << log::keyword_style("let") << ' ';
    ptrn->print(p);
    if (init) {
        p << " = ";
        init->print(p);
    }
    p << ';';
}

void ImplicitDecl::print(Printer& p) const {
    NodeScope _(p, "ImplicitDecl");
    p << log::keyword_style("implicit");
    if (type) {
        p << ' ';
        type->print(p);
    }
    p << " = ";
    value->print(p);
    p << ';';
}

void StaticDecl::print(Printer& p) const {
    NodeScope _(p, "StaticDecl");
    if (attrs) attrs->print(p);
    p << log::keyword_style("static") << ' ';
    if (is_mut)
        p << log::keyword_style("mut") << ' ';
    p << id.name;
    if (type) {
        p << ": ";
        type->print(p);
    }
    if (init) {
        p << " = ";
        init->print(p);
    }
    p << ';';
}

void FnDecl::print(Printer& p) const {
    NodeScope _(p, "FnDecl");
    if (attrs) attrs->print(p);
    p << log::keyword_style("fn") << ' ';
    if (fn->filter)
       fn->filter->print(p);
    p << id.name;

    if (type_params) type_params->print(p);
    if (fn->param) print_parens(p, fn->param);

    if (fn->ret_type) {
        p << " -> ";
        fn->ret_type->print(p);
    }

    if (fn->body) {
        if (fn->body->isa<BlockExpr>())
            p << ' ';
        else
            p << " = ";
        fn->body->print(p);
        if (!fn->body->isa<BlockExpr>())
            p << ';';
    } else
        p << ';';
}

void FieldDecl::print(Printer& p) const {
    NodeScope _(p, "FieldDecl");
    if (!id.name.empty())
        p << id.name << ": ";
    type->print(p);
    if (init) {
        p << " = ";
        init->print(p);
    }
}

inline void print_fields(Printer& p, const PtrVector<FieldDecl>& fields, bool is_tuple_like) {
    p << (is_tuple_like ? "(" : " {");
    if (!fields.empty()) {
        if (!is_tuple_like)
            p << p.indent();
        print_list(p, is_tuple_like ? ", " : ",", fields, [&] (auto& f) {
            if (!is_tuple_like)
                p << p.endl();
            f->print(p);
        });
        if (!is_tuple_like)
            p << p.unindent() << p.endl();
    }
    p << (is_tuple_like ? ")" : "}");
}

void StructDecl::print(Printer& p) const {
    NodeScope _(p, "StructDecl");
    if (attrs) attrs->print(p);
    p << log::keyword_style("struct") << ' ' << id.name;
    if (type_params) type_params->print(p);
    if (!is_tuple_like || !fields.empty())
        print_fields(p, fields, is_tuple_like);
    if (is_tuple_like)
        p << ";";
}

void OptionDecl::print(Printer& p) const {
    NodeScope _(p, "OptionDecl");
    p << id.name;
    if (param)
        print_parens(p, param);
    else if (fields.size() > 0)
        print_fields(p, fields, false);
}

void EnumDecl::print(Printer& p) const {
    NodeScope _(p, "EnumDecl");
    if (attrs) attrs->print(p);
    p << log::keyword_style("enum") << ' ' << id.name;
    if (type_params) type_params->print(p);
    p << " {";
    if (!options.empty()) {
        p << p.indent();
        print_list(p, ',', options, [&] (auto& o) {
            p << p.endl();
            o->print(p);
        });
        p << p.unindent() << p.endl();
    }
    p << '}';
}

void TypeDecl::print(Printer& p) const {
    NodeScope _(p, "TypeDecl");
    if (attrs) attrs->print(p);
    p << log::keyword_style("type") << ' ' <<  id.name;
    if (type_params) type_params->print(p);
    p << " = ";
    aliased_type->print(p);
    p << ';';
}

void ModDecl::print(Printer& p) const {
    NodeScope _(p, "ModDecl");
    if (attrs) attrs->print(p);
    bool anon = id.name == "";
    if (!anon)
        p << log::keyword_style("mod") << ' ' << id.name << " {" << p.indent() << p.endl();
    print_list(p, p.endl(), decls, [&] (auto& decl) {
        decl->print(p);
    });
    if (!anon)
        p << p.unindent() << p.endl() << "}";
}

void UseDecl::print(Printer& p) const {
    NodeScope _(p, "UseDecl");
    p << log::keyword_style("use") << ' ';
    path.print(p);
    if (id.name != "")
        p << ' ' << log::keyword_style("as") << ' ' << id.name;
    p << ';';
}

void ErrorDecl::print(Printer& p) const {
    NodeScope _(p, "ErrorDecl");
    p << log::error_style("<invalid declaration>");
}

void PrimType::print(Printer& p) const {
    p << log::keyword_style(tag_to_string(tag));
}

void TupleType::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& arg) {
        arg->print(p);
    });
    p << ')';
}

void SizedArrayType::print(Printer& p) const {
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    elem->print(p);
    p << " * ";
    std::visit([&] (auto&& arg) {
            using T = std::decay_t<decltype(arg)>;
            if constexpr (std::is_same_v<T, size_t>)
                p << arg;
            else if constexpr (std::is_same_v<T, ast::Path&>)
                arg->print(p);
    }, size);
    p << ']';
}

void UnsizedArrayType::print(Printer& p) const {
    p << '[';
    elem->print(p);
    p << ']';
}

void FnType::print(Printer& p) const {
    p << log::keyword_style("fn") << ' ';
    print_parens(p, from);
    p << " -> ";
    to->print(p);
}

void PtrType::print(Printer& p) const {
    p << '&';
    if (is_mut)
        p << log::keyword_style("mut") << ' ';
    if (addr_space != 0)
        p << log::keyword_style("addrspace") << '(' << addr_space << ')';
    if (pointee->isa<PtrType>())
        p << '(';
    pointee->print(p);
    if (pointee->isa<PtrType>())
        p << ')';
}

void TypeApp::print(Printer& p) const {
    NodeScope _(p, "TypeApp");
    path.print(p);
}

void NoCodomType::print(artic::Printer& p) const {
    NodeScope _(p, "artic");
    p << "!";
}

void ErrorType::print(Printer& p) const {
    p << log::error_style("<invalid type>");
}

log::Output& operator << (log::Output& out, const Node& node) {
    Printer p(out);
    node.print(p);
    return out;
}

void Node::dump() const {
    Printer p(log::out);
    p.show_implicit_casts = true;
    print(p);
    p << '\n';
}

} // namespace ast

// Types ---------------------------------------------------------------------------

void PrimType::print(Printer& p) const {
    p << log::keyword_style(ast::PrimType::tag_to_string(tag));
}

void TupleType::print(Printer& p) const {
    p << '(';
    print_list(p, ", ", args, [&] (auto& a) {
        a->print(p);
    });
    p << ')';
}

void SizedArrayType::print(Printer& p) const {
    if (is_simd)
        p << log::keyword_style("simd");
    p << '[';
    elem->print(p);
    p << " * " << size << ']';
}

void UnsizedArrayType::print(Printer& p) const {
    p << '[';
    elem->print(p);
    p << ']';
}

void PtrType::print(Printer& p) const {
    p << '&';
    if (is_mut)
        p << log::keyword_style("mut") << ' ';
    if (addr_space != 0)
        p << log::keyword_style("addrspace") << '(' << addr_space << ')';
    if (pointee->isa<PtrType>())
        p << '(';
    pointee->print(p);
    if (pointee->isa<PtrType>())
        p << ')';
}

void RefType::print(Printer& p) const {
    if (is_mut)
        p << "mutable ";
    p << "reference to ";
    pointee->print(p);
}

void ImplicitParamType::print(artic::Printer& p) const {
    NodeScope _(p, "artic");
    p << "implicit ";
    underlying->print(p);
}

void FnType::print(Printer& p) const {
    p << log::keyword_style("fn") << ' ';
    if (!dom->isa<TupleType>()) p << '(';
    dom->print(p);
    if (!dom->isa<TupleType>()) p << ')';
    p << " -> ";
    codom->print(p);
}

void BottomType::print(Printer& p) const {
    p << log::keyword_style("bottom");
}

void TopType::print(Printer& p) const {
    p << log::keyword_style("top");
}

void NoRetType::print(Printer& p) const {
    p << '!';
}

void TypeError::print(Printer& p) const {
    NodeScope _(p, "TypeError");
    p << log::error_style("<invalid type>");
}

void TypeVar::print(Printer& p) const {
    NodeScope _(p, "TypeVar");
    p << decl.id.name;
}

void ForallType::print(Printer& p) const {
    assert(decl.type_params);
    p << log::keyword_style("forall");
    decl.type_params->print(p);
    p << ' ';
    body->print(p);
}

void StructType::print(Printer& p) const {
    p << decl.id.name;
}

void EnumType::print(Printer& p) const {
    p << decl.id.name;
}

void ModType::print(Printer& p) const {
    p << decl.id.name;
}

void TypeAlias::print(Printer& p) const {
    NodeScope _(p, "TypeAlias");
    p << decl.id.name;
}

void TypeApp::print(Printer& p) const {
    NodeScope _(p, "TypeApp");
    applied->print(p);
    p << '[';
    print_list(p, ", ", type_args, [&] (auto& a) {
        a->print(p);
    });
    p << ']';
}

log::Output& operator << (log::Output& out, const Type& type) {
    Printer p(out);
    type.print(p);
    return out;
}

void Type::dump() const {
    Printer p(log::out);
    print(p);
    p << '\n';
}

} // namespace artic
