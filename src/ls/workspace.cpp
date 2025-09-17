#include "artic/ls/workspace.h"
#include "artic/log.h"

#include <algorithm>
#include <fstream>
#include <filesystem>
#include <fnmatch.h>
#include <iomanip>
#include <iostream>
#include <memory>
#include <optional>
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
    if(path.starts_with("~")) {
        static const char* home = std::getenv("HOME");
        if(home) return std::filesystem::path(home) / path.substr(1);
        else return "";
    }
    if(path.starts_with("/"))
        return path;

    return base_dir/path;
}

static std::optional<ConfigDocument> parse_config(const std::filesystem::path& config_path, ConfigLog& log) {
    log.file_context = config_path;
    if(config_path.empty()){
        log.error("Config file path is empty", "include-projects");
        return std::nullopt;
    }
    if (!std::filesystem::exists(config_path)) {
        log.error("Config file does not exist: " + config_path.string(), config_path.string());
        return std::nullopt;
    }
    try {
        nlohmann::json j; 
        std::ifstream is(config_path);
        is >> j;

        ConfigDocument doc;
        doc.path = config_path;
        if (!j.contains("artic-config")) {
            log.error(
                "Missing artic-config header in " + config_path.string()
                + "\nExample: " + nlohmann::json{{"artic-config", "1.0"}}.dump(),
                "projects"
            );
            return std::nullopt;
        }
        doc.version = j["artic-config"].get<std::string>();
        if (doc.version != "1.0") {
            log.warn("Unsupported artic-config version in " + config_path.string(), "artic-config");
        }

        auto parse_project = [&](const nlohmann::json& pj) -> std::optional<ProjectDefinition> {
            ProjectDefinition p;
            if (!pj.contains("name")) { 
                log.error(
                    "Project without name in " + config_path.string()
                    + "\nExample: " + nlohmann::json{{"name", "my_project"}}.dump(),
                    "projects"
                );
                return std::nullopt;
            }
            p.name = pj["name"].get<std::string>();

            p.root_dir =      pj.value<std::string>("folder", "");
            p.dependencies =  pj.value<std::vector<std::string>>("dependencies", {});
            p.file_patterns = pj.value<std::vector<std::string>>("files", {});
            p.origin = config_path;
            return p;
        };

        if (auto pj = j.find("projects"); pj != j.end()) {
            for (auto& pj : *pj) {
                if(auto pd = parse_project(pj)){
                    doc.projects.push_back(pd.value());
                }
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
                include.path = std::filesystem::weakly_canonical(include.path);
                include.raw_path_string = path;

                doc.includes.push_back(std::move(include));
            }
            if(missing_global) {
                log.warn("'include-projects' should contain '<global>' to improve readability as global projects are implicitly included", "include-projects");
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
    ConfigLog& log;
    std::map<Project::Identifier, config::ProjectDefinition>& projects;
    std::unordered_set<std::filesystem::path> visited_configs;
};

static void collect_projects_recursive(const config::ConfigDocument& config, CollectProjectsData& data, int depth = 0) {
    if(data.visited_configs.contains(config.path)) return;
    data.visited_configs.insert(config.path);

    auto& log = data.log;
    
    log.file_context = config.path;

    // Register Projects
    for (const auto& proj : config.projects) {
        if(data.projects.contains(proj.name)) {
            // already registered
            auto& val = data.projects[proj.name];
            if(val.origin == proj.origin) {
                if(depth < val.depth) {
                    val.depth = depth;
                    continue;
                }
            }
            log.warn("ignoring duplicate definition of " + proj.name + " in " + proj.origin.string(), proj.name);
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

        
        if(auto include_config = config::parse_config(include.path, log)) {
            log.file_context = config.path;
            collect_projects_recursive(include_config.value(), data, depth+1);
            
            log.info("Path: " + include.path.string(), include.raw_path_string);
        }
    }
}

static int match_pattern(
    const std::string& pattern, bool recursive, 
    const std::filesystem::path& root_dir, 
    std::unordered_set<std::filesystem::path>& matched_files,
    ConfigLog& log
) {
    std::error_code ec;
    std::filesystem::path base_dir = root_dir;
    std::string glob = pattern;

    // Expand ~ to $HOME
    if (!glob.empty() && glob[0] == '~') {
        const char* home = std::getenv("HOME");
        if (home) {
            glob = std::string(home) + glob.substr(1);
            base_dir = "";
        }
    }

    // If pattern is relative, resolve to base_dir
    std::filesystem::path search_path = base_dir;
    if (!glob.empty() && glob[0] == '/') {
        search_path = "";
    }

    // Split pattern into directory and filename part
    auto pos = glob.find_last_of("/\\");
    std::string dir_part = pos != std::string::npos ? glob.substr(0, pos) : "";
    std::string file_part = pos != std::string::npos ? glob.substr(pos + 1) : glob;

    // Handle recursive **
    bool has_recursive = glob.find("**") != std::string::npos;
    std::filesystem::path walk_dir = search_path / dir_part;

    int matched_count = 0;
    auto match_entry = [&](const std::filesystem::directory_entry& entry){
        if (!entry.is_regular_file()) return;
        auto rel = std::filesystem::relative(entry.path(), root_dir, ec);
        if (fnmatch(glob.c_str(), rel.string().c_str(), 0) == 0) {
            matched_files.insert(entry.path());
            matched_count++;
        }
    };
    if (has_recursive) {
        // Only one ** allowed
        if (std::count(glob.begin(), glob.end(), '*') > 2) {
            log.warn("Multiple recursive wildcards (**) are not supported in pattern: " + pattern, pattern);
            return 0;
        }
        // Remove ** from dir_part for walking
        auto recursive_pos = dir_part.find("**");
        std::filesystem::path recursive_base = walk_dir;
        if (recursive_pos != std::string::npos) {
            recursive_base = search_path / dir_part.substr(0, recursive_pos);
        }
        for (auto& entry : std::filesystem::recursive_directory_iterator(recursive_base, ec)) {
            match_entry(entry);
        }
    } else {
        // Non-recursive: walk only one directory
        std::filesystem::path dir_to_walk = walk_dir.empty() ? search_path : walk_dir;
        if (std::filesystem::exists(dir_to_walk)) {
            for (auto& entry : std::filesystem::directory_iterator(dir_to_walk, ec)) {
                match_entry(entry);
            }
        }
    }

    return matched_count;
}

static std::shared_ptr<Project> instantiate_project(
    const config::ProjectDefinition& proj_def, 
    std::unordered_map<std::filesystem::path, std::shared_ptr<File>>& tracked_files,
    ConfigLog& log
) {
    log.file_context = proj_def.origin;

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

    // Evaluate include patterns
    for (const auto& pattern : include_patterns) {
        bool recursive = pattern.find("**") != std::string::npos;
        int matched_count = match_pattern(pattern, recursive, root_dir, matched_files, log);
        // log::debug("Pattern {} matched {} files", pattern, matched_count);
        if (matched_count == 0) {
            log.warn("0 files", pattern);
        } else {
            log.info(std::to_string(matched_count) + " files matched", pattern);
        }
    }

    auto file_arr_to_string = [](const auto& files, const std::filesystem::path& root_dir){
        std::ostringstream s;
        s << files.size() << " files:" << std::endl;
        for(const auto& file : files) {
            s << "- " << std::filesystem::relative(file, root_dir).string() << " " << std::endl;
        }
        return s.str();
    };
    // log.info(file_arr_to_string(matched_files, root_dir), proj_def.name);

    // Remove files matching any exclude pattern
    // TODO improve
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
    project->origin = proj_def.origin;

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

void Workspace::reload(ConfigLog& log) {
    projects_ = ProjectRegistry();

    std::map<Project::Identifier, config::ProjectDefinition> project_defs;
    std::optional<config::ProjectDefinition> default_project;
    CollectProjectsData data {.log = log, .projects=project_defs };

    log.file_context = workspace_config_path;

    // Discover projects from global config recursively
    if(!global_config_path.empty()){
        if(auto global_config = config::parse_config(global_config_path, log)) {
            collect_projects_recursive(global_config.value(), data);
            if(auto& dp = global_config->default_project){
                default_project = dp.value();
            }
        }
        log.info("global config: " + global_config_path.string(), "<global>");
    } else {
        log.warn("Could not find global config file", "<global>");
    }
        
    // Discover projects from local config recursively
    if(!workspace_config_path.empty())
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
            .project = instantiate_project(def, projects_.tracked_files, log), 
            .dependencies = std::move(def.dependencies)
        };
    }
    
    // resolve dependencies
    for (auto& [id, p] : projects) {
        for (auto& dep_id : p.dependencies) {
            if(projects.contains(dep_id)) {
                auto& dep = projects.at(dep_id).project;
                p.project->dependencies.push_back(dep);
            } 
        }

        projects_.all_projects.push_back(p.project);
    }

    // log dependency resolution
    
    for (auto& [id, p] : projects) {     
        auto log_project_info = [&, &origin=p.project->origin](const Project& dep){
            auto files = dep.collect_files();
            std::ostringstream s;
            auto num_own_files = dep.files.size();
            auto dep_files = files.size() - num_own_files;
            s << num_own_files; 
            if(dep_files > 0) s << " + " << dep_files;
            s << " files | ";
            if(dep.origin == origin)
                s << "declared in this config";
            else
                s << "declared in config " << "\"" << dep.origin.string() << "\"";
            s << " | files: " << std::endl;
            for(const auto& file : files) {
                s << "- " << "\"" << std::filesystem::weakly_canonical(file->path).string() << "\" " << std::endl;
            }
            log.info(s.str(), dep.name);
        };

        log.file_context = p.project->origin;
        log_project_info(*p.project);

        for (auto& dep_id : p.dependencies) {
            if(projects.contains(dep_id)) {
                auto& dep = projects.at(dep_id).project;
                log_project_info(*dep);
                p.project->dependencies.push_back(dep);   
            } else {
                log.error("failed to resolve dependency " + dep_id + " for project " + p.project->name, p.project->name);
            }
        }

        projects_.all_projects.push_back(p.project);
    }

    // register default project
    if(auto& dp = default_project){
        auto project = instantiate_project(dp.value(), projects_.tracked_files, log);
        for (auto& dep_id : dp->dependencies) {
            if(!projects.contains(dep_id)) {
                log.file_context = dp->origin;
                log.error("failed to resolve dependency " + dep_id + " for project " + project->name, dep_id);
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

    log.file_context = "";
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
            indent(ind+1);
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