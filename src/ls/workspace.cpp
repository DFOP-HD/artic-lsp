#include "artic/ls/workspace.h"
#include "artic/log.h"

#include <fstream>
#include <filesystem>
#include <fnmatch.h>
#include <iostream>
#include <memory>
#include <optional>
#include <vector>
#include <unordered_set>

#include <nlohmann/json.hpp>

namespace artic::ls::workspace {

// File ----------------------------------------------------------------------

static std::optional<std::string> read_file(const std::string& file) {
    std::ifstream is(file);
    if (!is)
        return std::nullopt;
    // Try/catch needed in case file is a directory (throws exception upon read)
    try {
        return std::make_optional(std::string(
            std::istreambuf_iterator<char>(is),
            std::istreambuf_iterator<char>()
        ));
    } catch (...) {
        return std::nullopt;
    }
}

void File::read() const {
    text = read_file(path);
    if (!text) {
        log::error("Could not read file {}", path);
    }
}

// Project --------------------------------------------------------------------

std::vector<const File*> Project::collect_files() const {
    std::unordered_set<const File*> result;
    for (const auto& file : files) {
        result.insert(file.get());
    }
    for (const auto& dependency : dependencies){
        auto dep_files = dependency->collect_files();
        result.insert(dep_files.begin(), dep_files.end());
    }
    std::vector res(result.begin(), result.end());
    return res;
}

bool Project::uses_file(const std::filesystem::path& file) const {
    for (const auto& f : files)
        if(f->path == file) return true;
    for (const auto& dep : dependencies)
        if(dep->uses_file(file)) return true;
    return false;
}

// Workspace --------------------------------------------------------------------

std::optional<std::shared_ptr<Project>> Workspace::project_for_file(const std::filesystem::path& file) const {
    // try active project
    if(active_project.has_value()){
        auto active = std::find_if(projects_.all_projects.begin(), projects_.all_projects.end(), [&](const auto& project){
            return project->name == active_project.value();
        });
        if(active != projects_.all_projects.end() && (*active)->uses_file(file)){
            return *active;
        }
    }

    // if not in active project, try next best project
    for (const auto& project : projects_.all_projects) {
        for (const auto& f : project->files) {
            // do not use recursive check, as dependencies are also contained in all_projects
            if (f->path == file) { 
                return project;
            }
        }
    }

    // no project can be found -> may need to use default project
    return std::nullopt;
}

// Project Registry --------------------------------------------------------------------

static inline void print_project(const Project& proj, int ind = 0){
    auto indent = [](int i){ 
        // log::Output out(std::clog, false);
        for (int j=0; j<i; j++)
            std::clog << "  ";
    };

    indent(ind);
    log::debug("project: '{}' (", proj.name);
    
    indent(ind+1);
    log::debug("files: (");
    for (const auto& file : proj.files) {
        indent(ind+2);
        log::debug("{}", file->path);
    }

    indent(ind+1);
    log::debug(")");
    
    indent(ind+1);
    log::debug("dependencies: (");
    for (const auto& dep : proj.dependencies) {
        const bool print_recursive = false;
        if(print_recursive)
            print_project(*dep, ind + 2);
        else {
            indent(ind+2);
            log::debug("project: '{}'", dep->name);
        }
    }
    indent(ind+1);
    log::debug(")");
    indent(ind);
    log::debug(")");
}

void ProjectRegistry::print() const {
    log::debug("--- Project Registry ---");
    print_project(*default_project);
    for (const auto& p : all_projects){
        print_project(*p);
    }
    log::debug("--- Project Registry ---");
    std::clog << std::endl;
}

} // namespace artic::ls