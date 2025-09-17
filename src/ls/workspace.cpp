#include "artic/ls/workspace.h"
#include "artic/log.h"

#include <algorithm>
#include <fstream>
#include <filesystem>
#include <fnmatch.h>
#include <iostream>
#include <memory>
#include <vector>
#include <unordered_map>
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

// Config --------------------------------------------------------------------

namespace config {

static std::filesystem::path to_absolute_path(const std::filesystem::path& base_dir, std::string_view path) {
    std::filesystem::path abs_path;
    if(path.starts_with("~")) {
        static const char* home = std::getenv("HOME");
        if(home) abs_path = std::filesystem::path(home) / path.substr(1);
    }
    if(!abs_path.is_absolute()){
        abs_path = base_dir / abs_path;
    }
    return abs_path;
}

static std::optional<ConfigDocument> parse_config(const std::filesystem::path& config_path, WorkspaceConfigLog& log) {
    if (!std::filesystem::exists(config_path)) {
        log.error("Config file does not exist: " + config_path.string());
        return std::nullopt;
    }
    try {
        nlohmann::json j; 
        std::ifstream is(config_path);
        is >> j;

        ConfigDocument doc;
        doc.path = config_path;
        // if (!j.contains("artic-config")) {
        //     log.error(
        //         "Missing artic-config header in " + path.string()
        //         + "\nExample: " + nlohmann::json{{"artic-config", "1.0"}}.dump()
        //     );
        //     return std::nullopt;
        // }
        doc.version = j["artic-config"].get<std::string>();
        if (doc.version != "1.0") {
            log.warn("Unsupported artic-config version in " + config_path.string());
        }

        auto parse_project = [&](const nlohmann::json& pj){
            ProjectDefinition p;
            // if (!pj.contains("name")) { 
            //     log.warn(
            //         "Project without name in " + path.string()
            //         + "\nExample: " + nlohmann::json{{"name", "my_project"}}.dump()
            //     );
            //     continue;
            // }
            p.name = pj["name"].get<std::string>();

            p.root_dir =      pj.value<std::string>("folder", "");
            p.dependencies =  pj.value<std::vector<std::string>>("dependencies", {});
            p.file_patterns = pj.value<std::vector<std::string>>("files", {});
            p.origin = config_path;
            return p;
        };

        if (auto pj = j.find("projects"); pj != j.end()) {
            for (auto& pj : *pj) {
                doc.projects.push_back(parse_project(pj));
            }
        }
        if (auto dpj = j.find("default-project"); dpj != j.end()) {
            doc.default_project = parse_project(*dpj);
        }
        if (j.contains("include-projects")) {
            bool missing_global = true;
            for (auto& incj : j["include-projects"]) {
                auto path = incj.get<std::string>();
                if(missing_global && path == "<global>"){
                    missing_global = false;
                    continue;
                }
                IncludeConfig include;
                if(path.ends_with('?')){
                    path = path.substr(0, path.size()-1);
                    include.is_optional = true;
                } 
                include.path = to_absolute_path(config_path.parent_path(), path);

                doc.includes.push_back(std::move(include));
            }
            if(missing_global) {
                log.warn("'include-projects' should contain '<global>' to improve readability as global projects are implicitly included");
            }
        }
        return doc;
    } catch (const std::exception& e) {
        log.error(std::string("Failed to parse ") + config_path.string() + ": " + e.what());
        return std::nullopt;
    }
}

} // config

// Workspace Helpers ----------------------------------------------------------------------

struct CollectProjectsData {
    WorkspaceConfigLog& log;
    std::map<Project::Identifier, config::ProjectDefinition>& projects;
    std::unordered_set<std::filesystem::path> visited_configs;
};

static void collect_projects_recursive(const config::ConfigDocument& config, CollectProjectsData& data, int depth = 0) {
    if(data.visited_configs.contains(config.path)) return;
    data.visited_configs.insert(config.path);

    // Register Projects
    for (const auto& proj : config.projects) {
        if(data.projects.contains(proj.name)) {
            // already registered
            auto& val = data.projects[proj.name];
            if(val.origin == proj.origin) {
                if(depth < val.depth) val.depth = depth;
            } else {
                data.log.warn("ignoring duplicate definition of " + proj.name + " in " + proj.origin.string());
            }
            continue;
        }

        data.projects[proj.name] = proj;
        data.projects[proj.name].depth = depth;
    }
    // Recurse included configs
    for (const auto& include : config.includes) {
        if (!std::filesystem::exists(include.path)) {
            if(include.is_optional) continue;
        } 
        if(auto include_config = config::parse_config(include.path, data.log)) {
            collect_projects_recursive(include_config.value(), data, depth+1);
        }
    }
}

static std::shared_ptr<Project> instantiate_project(
    const config::ProjectDefinition& proj_def, 
    std::unordered_map<std::filesystem::path, std::shared_ptr<File>>& tracked_files
) {
    // evaluate file patterns
    std::vector<std::string> include_patterns;
    std::vector<std::string> exclude_patterns;
    for (const auto& pattern : proj_def.file_patterns) {
        if (!pattern.empty() && pattern[0] == '!') {
            exclude_patterns.push_back(pattern.substr(1));
        } else {
            include_patterns.push_back(pattern);
        }
    }

    std::filesystem::path root_dir = proj_def.origin.parent_path();

    // Collect all files matching include patterns and not matching exclude patterns
    std::unordered_set<std::filesystem::path> matched_files;

    // TODO add support for ~/ path
    auto match_pattern = [&](const std::string& pattern, bool recursive) {
        std::filesystem::path base = root_dir;
        std::string glob = pattern;
        // Handle patterns like "src/**/*.cpp"
        auto pos = pattern.find('/');
        if (pos != std::string::npos && pattern.find("**") == pos) {
            // "**/foo.cpp" or similar
            glob = pattern.substr(pos + 1);
        }
        std::error_code ec;
        if (recursive) {
            for (auto& entry : std::filesystem::recursive_directory_iterator(base, ec)) {
                if (entry.is_regular_file()) {
                    auto rel = std::filesystem::relative(entry.path(), root_dir, ec);
                    if (fnmatch(glob.c_str(), rel.string().c_str(), 0) == 0) {
                        matched_files.insert(entry.path());
                    }
                }
            }
        } else {
            for (auto& entry : std::filesystem::directory_iterator(base, ec)) {
                if (entry.is_regular_file()) {
                    auto rel = std::filesystem::relative(entry.path(), root_dir, ec);
                    if (fnmatch(glob.c_str(), rel.string().c_str(), 0) == 0) {
                        matched_files.insert(entry.path());
                    }
                }
            }
        }
    };

    // Evaluate include patterns
    for (const auto& pattern : include_patterns) {
        bool recursive = pattern.find("**") != std::string::npos;
        match_pattern(pattern, recursive);
    }

    // Remove files matching any exclude pattern
    for (const auto& pattern : exclude_patterns) {
        bool recursive = pattern.find("**") != std::string::npos;
        std::vector<std::filesystem::path> to_remove;
        auto glob = pattern;
        std::error_code ec;
        if (recursive) {
            for (const auto& file : matched_files) {
                auto rel = std::filesystem::relative(file, root_dir, ec);
                if (fnmatch(glob.c_str(), rel.string().c_str(), 0) == 0) {
                    to_remove.push_back(file);
                }
            }
        } else {
            for (const auto& file : matched_files) {
                auto rel = std::filesystem::relative(file, root_dir, ec);
                if (fnmatch(glob.c_str(), rel.string().c_str(), 0) == 0) {
                    to_remove.push_back(file);
                }
            }
        }
        for (const auto& file : to_remove) {
            matched_files.erase(file);
        }
    }

    auto project = std::make_shared<Project>();
    project->name = proj_def.name;

    // Assign files to the project
    for (const auto& file : matched_files) {
        if(tracked_files.contains(file)) {
            project->files.push_back(tracked_files[file]);
        } else {
            auto file_ptr = std::make_shared<File>(file);
            tracked_files[file] = file_ptr;
            project->files.push_back(file_ptr);
        }
    }

    return project;
}

// Workspace --------------------------------------------------------------------------

void Workspace::reload(WorkspaceConfigLog& log) {
    projects_ = ProjectRegistry();

    std::map<Project::Identifier, config::ProjectDefinition> project_defs;
    std::optional<config::ProjectDefinition> default_project;
    CollectProjectsData data {.log = log, .projects=project_defs };

    // Discover projects from global config recursively
    if(auto global_config = config::parse_config(global_config_path, log)) {
        collect_projects_recursive(global_config.value(), data);
        if(auto& dp = global_config->default_project){
            default_project = dp.value();
        }
    }

    // Discover projects from local config recursively
    if(auto local_config = config::parse_config(workspace_config_path, log)) {
        collect_projects_recursive(local_config.value(), data);
        if(auto& dp = local_config->default_project){
            default_project = dp.value();
        }
        if(local_config->projects.empty()) {
            active_project = std::nullopt;
        } else {
            active_project = local_config->projects.front().name;
        }
    }

    struct IntermediateProject {
        std::shared_ptr<Project> project;
        std::vector<Project::Identifier> dependencies;
    };

    std::map<Project::Identifier, IntermediateProject> projects;

    // convert project definitions to project instances
    for (const auto& [id, def] : project_defs) {
        projects[id] = {
            .project = instantiate_project(def, projects_.tracked_files), 
            .dependencies = std::move(def.dependencies)
        };
    }
    
    // resolve dependencies
    for (auto& [id, p] : projects) {
        for (auto& dep_id : p.dependencies) {
            if(!projects.contains(dep_id)) {
                log.error("failed to resolve dependency " + dep_id + " for project " + p.project->name);
                continue;
            }
            auto& dep = projects.at(dep_id).project;
            p.project->dependencies.push_back(dep);        
        }

        projects_.all_projects.push_back(p.project);
    }

    // register default project
    if(auto& dp = default_project){
        auto project = instantiate_project(dp.value(), projects_.tracked_files);
        for (auto& dep_id : dp->dependencies) {
            if(!projects.contains(dep_id)) {
                log.error("failed to resolve dependency " + dep_id + " for project " + project->name);
                continue;
            }
            auto& dep = projects.at(dep_id).project;
            project->dependencies.push_back(dep);        
        }

        projects_.default_project = project;
    } else {
        auto project = std::make_shared<Project>();
        project->name = "<no project>";
        project->files = {};
        project-> dependencies = {};
        projects_.default_project = project;
    }
}

bool Workspace::is_file_part_of_project(const Project& project, const std::filesystem::path& file) const {
    for (const auto& file : project.collect_files()) {
        if(file->path == file->path) return true;
    }
    return false;
}

std::optional<std::shared_ptr<Project>> Workspace::project_for_file(const std::filesystem::path& file) const {
    // try active project
    if(active_project.has_value()){
        auto active = std::find_if(projects_.all_projects.begin(), projects_.all_projects.end(), [&](const auto& project){
            if(project->name == active_project.value()){
                return true;
            }
            return false;
        });
        if(active != projects_.all_projects.end()){
            if(is_file_part_of_project(**active, file))
                return *active;
        }
    }

    // if not in active project, try next best project
    for (const auto& project : projects_.all_projects) {
        for (const auto& f : project->files) {
            if (f->path == file) {
                return project;
            }
        }
    }

    // no project can be found
    return std::nullopt;
}

std::vector<const File*> Project::collect_files() const {
    std::vector<const File*> result;
    for (const auto& file : files) {
        result.push_back(file.get());
    }
    for (const auto& dependency : dependencies){
        auto dep_files = dependency->collect_files();
        result.insert(result.end(), dep_files.begin(), dep_files.end());
    }
    return result;
}

static inline void print_project(const Project& proj, int indent = 0){
    auto newline = [indent](int i){log::debug("\n{}", std::string(' ', (indent + i)*4));};

    newline(indent);
    log::debug("project: '{}' {", proj.name);
    
    newline(indent);
    log::debug("files: {");
    for (const auto& file : proj.files) {
        newline(indent+1);
        log::debug("- {}", file->path);
    }
    newline(indent);
    log::debug("}");
    newline(indent);
    log::debug("dependencies: {");
    for (const auto& dep : proj.dependencies) {
        print_project(*dep, indent + 1);
        newline(indent);
    }
    log::debug("}");
    newline(indent);
    log::debug("}");
    newline(indent);
}

void ProjectRegistry::print() const {
    // log::debug("default project: ", )
}

} // namespace artic::ls