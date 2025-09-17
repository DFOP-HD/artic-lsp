#include "artic/ls/compile.h"

#include "artic/parser.h"
#include "artic/locator.h"
#include "artic/bind.h"
#include "artic/check.h"
#include "artic/summoner.h"
#include <iostream>

namespace {

struct MemBuf : public std::streambuf {
    MemBuf(const std::string& str) {
        setg(
            const_cast<char*>(str.data()),
            const_cast<char*>(str.data()),
            const_cast<char*>(str.data() + str.size()));
    }

    std::streampos seekoff(std::streamoff off, std::ios_base::seekdir way, std::ios_base::openmode) override {
        if (way == std::ios_base::beg)
            setg(eback(), eback() + off, egptr());
        else if (way == std::ios_base::cur)
            setg(eback(), gptr() + off, egptr());
        else if (way == std::ios_base::end)
            setg(eback(), egptr() + off, egptr());
        else
            return std::streampos(-1);
        return gptr() - eback();
    }

    std::streampos seekpos(std::streampos pos, std::ios_base::openmode mode) override {
        return seekoff(std::streamoff(pos), std::ios_base::beg, mode);
    }

    std::streamsize showmanyc() override {
        return egptr() - gptr();
    }
};

} // anonymous namespace

namespace artic::ls::compiler {

std::unique_ptr<CompileResult> compile_files(std::span<const workspace::File*> files, const std::shared_ptr<CompilerInstance>& compiler) {
    auto program = compiler->arena.make_ptr<ast::ModDecl>();

    for (auto& file : files){
        file->read();
        if (!file->text) {
            log::error("cannot open file '{}'", file->path);
            return std::make_unique<CompileResult>(compiler, nullptr, CompileResult::Invalid);
        }
        if (compiler->log.locator)
            compiler->log.locator->register_file(file->path, file->text.value());

        MemBuf mem_buf(file->text.value());
        std::istream is(&mem_buf);

        Lexer lexer(compiler->log, file->path, is);
        Parser parser(compiler->log, lexer, compiler->arena);
        parser.warns_as_errors = compiler->warns_as_errors;
        auto module = parser.parse();

        if(compiler->log.errors > 0) {
            log::error("Parsing failed");
            return std::make_unique<CompileResult>(compiler, std::move(program), CompileResult::Invalid);
        } else {
            // log::debug("Parsing completed successfully");
        }

        program->decls.insert(
            program->decls.end(),
            std::make_move_iterator(module->decls.begin()),
            std::make_move_iterator(module->decls.end())
        );
    }

    program->set_super();

    NameBinder name_binder(compiler->log);
    name_binder.warns_as_errors = compiler->warns_as_errors;
    if (compiler->enable_all_warns)
        name_binder.warn_on_shadowing = true;

    TypeChecker type_checker(compiler->log, compiler->type_table, compiler->arena);
    type_checker.warns_as_errors = compiler->warns_as_errors;

    Summoner summoner(compiler->log, compiler->arena);

    if (!name_binder.run(*program))
        return std::make_unique<CompileResult>(compiler, std::move(program), CompileResult::Parsed);
    if(!type_checker.run(*program))
        return std::make_unique<CompileResult>(compiler, std::move(program), CompileResult::NameBinded);
    if(!summoner.run(*program))
        return std::make_unique<CompileResult>(compiler, std::move(program), CompileResult::TypeChecked);

    return std::make_unique<CompileResult>(compiler, std::move(program), CompileResult::Valid);
}

} // namespace artic::ls::compiler
