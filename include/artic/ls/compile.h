#ifndef ARTIC_LS_COMPILE_H
#define ARTIC_LS_COMPILE_H

#include "artic/arena.h"
#include "artic/bind.h"
#include "artic/ls/workspace.h"
#include "artic/types.h"
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
        , name_map(std::make_unique<NameMap>())
        , name_binder(log, name_map.get()) 
    {
        log.max_errors = 100;
    }

    std::unique_ptr<CompileResult> compile_files(std::span<const workspace::File*> files);

    Arena arena;
    TypeTable type_table;
    Locator locator;
    Log log;

    std::unique_ptr<NameMap> name_map;
    NameBinder name_binder;

    bool warns_as_errors = false;
    bool enable_all_warns = true;
};

struct CompileResult {
    enum Stage {
        Invalid     = 0,
        Parsed      = 1,
        NameBinded  = 2,
        TypeChecked = 3,
        Summoned    = 4,
        Valid       = 4,
    }; 

    // Output -----
    Ptr<ast::ModDecl> program;
    Stage stage = Invalid;
    
    // Input -----
    std::shared_ptr<CompilerInstance> compiler; // used to keep compiler alive after compilation TODO make unique_ptr
    std::vector<std::unique_ptr<workspace::File>> temporary_files; // used to keep temporary file alive after compilation
    std::filesystem::path active_file; // used for recompilation when the configuration changes. Could be done in a cleaner way

    CompileResult(Ptr<ast::ModDecl> program, Stage stage)
        : program(program.get()), stage(stage)
    {}
};

} // namespace artic::ls::compiler

#endif // ARTIC_LS_COMPILE_H
