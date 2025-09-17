#ifndef ARTIC_LS_COMPILE_H
#define ARTIC_LS_COMPILE_H

#include "artic/arena.h"
#include "artic/ls/workspace.h"
#include "artic/types.h"
#include "artic/locator.h"
#include "artic/log.h"
#include <span>

namespace artic::ls::compiler {

struct CompilerInstance {
    Arena arena;
    TypeTable type_table;
    Locator locator;
    Log log;
    bool warns_as_errors = false;
    bool enable_all_warns = true;
    
    CompilerInstance() 
        : arena(), type_table(), locator(), log(log::err, &locator) 
    {
        log.max_errors = 100;
    }
};

struct CompileResult {
    Ptr<ast::ModDecl> program;
    enum Stage {
        Invalid     = 0,
        Parsed      = 1,
        NameBinded  = 2,
        TypeChecked = 3,
        Summoned    = 4,
        Valid       = 4,
    } stage = Invalid;

    std::shared_ptr<CompilerInstance> compiler;

    CompileResult(std::shared_ptr<CompilerInstance> compiler, Ptr<ast::ModDecl> program, Stage stage)
        : program(program.get()), stage(stage), compiler(std::move(compiler)) 
    {}
};

std::unique_ptr<CompileResult> compile_files(std::span<const workspace::File*> files, const std::shared_ptr<CompilerInstance>& compiler);

} // namespace artic::ls::compiler

#endif // ARTIC_LS_COMPILE_H
