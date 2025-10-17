#ifndef ARTIC_LS_COMPILE_H
#define ARTIC_LS_COMPILE_H

#include "artic/arena.h"
#include "artic/bind.h"
#include "artic/ls/workspace.h"
#include "artic/types.h"
#include "artic/check.h"
#include "artic/locator.h"
#include "artic/log.h"
#include <memory>
#include <span>

namespace artic::ls::compiler {

struct CompileResult;

struct CompilerInstance {
    CompilerInstance()
        : arena(), type_table(), locator()
        , log(log::err, &locator)
        , name_binder(log, &name_map)
        , type_checker(log, type_table, arena, &name_map, &type_hints)
    {
        log.max_errors = 100;
        type_checker.warns_as_errors = warns_as_errors;
        name_binder.warns_as_errors = warns_as_errors;
        if (enable_all_warns)
            name_binder.warn_on_shadowing = true;
    }

    std::unique_ptr<CompileResult> compile_files(std::span<const workspace::File*> files);

    NameMap name_map;
    TypeHints type_hints;

    Arena arena;
    TypeTable type_table;
    Locator locator;
    Log log;

    NameBinder name_binder;
    TypeChecker type_checker;

    bool warns_as_errors = false;
    bool enable_all_warns = true;
};

struct CompileResult {
    // Output -----
    Ptr<ast::ModDecl> program;
    
    // Input -----
    std::shared_ptr<CompilerInstance> compiler; // used to keep compiler alive after compilation TODO make unique_ptr
    std::vector<std::unique_ptr<workspace::File>> temporary_files; // used to keep temporary file alive after compilation
    std::filesystem::path active_file; // used for recompilation when the configuration changes. Could be done in a cleaner way

    explicit CompileResult(Ptr<ast::ModDecl> program)
        : program(program.get())
    {}
};

} // namespace artic::ls::compiler

#endif // ARTIC_LS_COMPILE_H
