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
#include <regex>

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

static std::optional<ConfigDocument> parse_config(const std::filesystem::path& config_path, ConfigLog& log, std::optional<std::string> raw_path_context = std::nullopt) {
    if(config_path.empty()){
        log.error("Config file path is empty", "include-projects");
        return std::nullopt;
    }
    if (!std::filesystem::exists(config_path)) {
        auto context = raw_path_context.value_or(config_path.string());
        log.error("Config file does not exist: \"" + config_path.string() + "\"", context);
        return std::nullopt;
    }
    log.file_context = config_path;
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
                include.raw_path_string = path;
                if(path.ends_with('?')){
                    path = path.substr(0, path.size()-1);
                    include.is_optional = true;
                } 
                include.path = to_absolute_path(config_path.parent_path(), path);
                include.path = std::filesystem::weakly_canonical(include.path);

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
        
        if(auto include_config = config::parse_config(include.path, log, include.raw_path_string)) {
            collect_projects_recursive(include_config.value(), data, depth+1);
            
            log.file_context = config.path;
            auto log_project_info = [&](const config::ConfigDocument& cfg){
                std::ostringstream s;

                s << cfg.projects.size() << " + ?" << " projects: ";
                s << std::endl; 
                for(const auto& file : cfg.projects) {
                    s << "- " << file.name << std::endl;
                }
                log.info(s.str(), include.raw_path_string);
            };
            log_project_info(include_config.value());
            log.info("Path: \"" + include.path.string() + "\"", include.raw_path_string);
        }
    }
}
// Convert a glob pattern (with *, **, ?, and path separators) into a regex.
// This is a simple translator that treats '/' as the path separator and
// translates:
//  - *  -> [^/]*          (any sequence of non-slash characters)
//  - ** -> .*              (any sequence across subdirectories)
//  - ?  -> [^/]            (single non-slash character)
//  - Other regex metacharacters are escaped
static inline std::string glob_to_regex(const std::string& glob) {
    std::string regex;
    regex.reserve(glob.size() * 2);

    // Helper to escape regex metacharacters
    auto escape = [](char c) -> std::string {
        switch (c) {
            case '.': case '+': case '^': case '$': case '(': case ')':
            case '|': case '{': case '}': case '[': case ']': case '\\':
                return "\\" + std::string(1, c);
            default:
                return std::string(1, c);
        }
    };

    // Normalize path separators to '/' for matching
    std::string g = glob;
    for (char& ch : g) {
        if (ch == '\\') ch = '/';
    }

    // Translate
    for (size_t i = 0; i < g.size(); ) {
        if (g.compare(i, 2, "**") == 0) {
            // ** -> match anything including separators
            regex += ".*";
            i += 2;
        } else if (g[i] == '*') {
            // * -> any sequence of non-slash characters
            regex += "[^/]*";
            ++i;
        } else if (g[i] == '?') {
            // ? -> any single non-slash character
            regex += "[^/]";
            ++i;
        } else {
            // Escape other characters
            regex += escape(g[i]);
            ++i;
        }
    }

    // Anchor to full string
    return "^" + regex + "$";
}

// Find all files under root matching the given glob pattern.
// The pattern is interpreted with '/' as the separator and can include
// *, **, ? as described.
static inline std::vector<std::filesystem::path> find_matching_glob(std::filesystem::path root, std::string glob_pattern, ConfigLog& log) {
    std::vector<std::filesystem::path> results;

    if (glob_pattern[0] == '/') {
        root = "";
    } else if (glob_pattern[0] == '~') {
        const char* home = std::getenv("HOME");
        if (home) {
            glob_pattern = glob_pattern.substr(1);
            root = home;
        } else {
            log.warn("Cannot expand ~ in pattern: " + glob_pattern + "$HOME is undefined", glob_pattern);
            return results;
        }
    }

    if (!std::filesystem::exists(root) || !std::filesystem::is_directory(root)) return results;

    // Convert glob to regex (based on path separators '/')
    std::string regex_str = glob_to_regex(glob_pattern);

    // Build a regex with case sensitivity aligned to your filesystem
    std::regex re(regex_str, std::regex::ECMAScript);

    // Walk recursively and test relative paths
    for (auto it = std::filesystem::recursive_directory_iterator(root); it != std::filesystem::recursive_directory_iterator(); ++it) {
        const auto& entry = *it;
        if (!entry.is_regular_file()) continue;

        std::filesystem::path full = entry.path();
        std::filesystem::path rel;
        try {
            rel = std::filesystem::relative(full, root);
        } catch (...) {
            continue;
        }

        // Normalize to POSIX-like form for matching
        std::string rel_str = rel.generic_string();

        // Check match
        if (std::regex_match(rel_str, re)) {
            results.push_back(full);
        }
    }

    return results;
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
            exclude_patterns.push_back(pattern);
        } else {
            include_patterns.push_back(pattern);
        }
    }

    std::filesystem::path root_dir = proj_def.origin.parent_path();

    // Collect all files matching include patterns and not matching exclude patterns
    std::unordered_set<std::filesystem::path> matched_files;

    auto file_arr_to_string = [](const std::filesystem::path& root_dir, const auto& files){
        std::ostringstream s;
        s << files.size() << " files:" << std::endl;
        for(const auto& file : files) {
            s << "- " << std::filesystem::relative(file, root_dir).string() << " " << std::endl;
        }
        return s.str();
    };

    // Evaluate include patterns
    for (const auto& pattern : include_patterns) {
        auto matches = find_matching_glob(root_dir, pattern, log);
        if (matches.empty()) {
            log.warn("empty pattern", pattern);
            continue;
        } 
        
        auto before = matched_files.size();
        matched_files.insert(matches.begin(), matches.end());
        auto after = matched_files.size();

        log.info(
            "+ " + std::to_string(after - before) + " files"
            + " | total matches: " + file_arr_to_string(root_dir, matches),
            pattern
        );
    }

    for (const auto& pattern : exclude_patterns) {
        auto matches = find_matching_glob(root_dir, pattern.substr(1), log);
        if (matches.empty()) {
            log.warn("empty exclude pattern", pattern);
            continue;
        } 
        auto before = matched_files.size();
        for (const auto& m : matches) {
            matched_files.erase(m);
        }
        auto after = matched_files.size();

        log.info(
            "- " + std::to_string(before - after) + " files"
            + " | total matches: " + file_arr_to_string(root_dir, matches),
            pattern
        );
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
            project->files.push_back(file_ptr);
            tracked_files[file] = file_ptr;
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

    auto log_project_info = [&](const Project& dep, const std::filesystem::path& current_config){
        log.file_context = current_config;
        if(dep.origin != current_config)
            log.info("Declared in config \"" + dep.origin.string() + "\"", dep.name);

        auto files = dep.collect_files();
        std::ostringstream s;
        auto num_own_files = dep.files.size();
        auto dep_files = files.size() - num_own_files;
        s << num_own_files; 
        if(dep_files > 0) s << " + " << dep_files;
        s << " files: " << std::endl;
        for(const auto& file : files) {
            s << "- " << "\"" << std::filesystem::weakly_canonical(file->path).string() << "\" " << std::endl;
        }
        log.info(s.str(), dep.name);

        if(dep.origin == current_config)
            log.info("Declared in this config", dep.name);
    };

    // log dependency resolution
    for (auto& [id, p] : projects) {     
        log.file_context = p.project->origin;
        log_project_info(*p.project, p.project->origin);

        for (auto& dep_id : p.dependencies) {
            if(projects.contains(dep_id)) {
                auto& dep = projects.at(dep_id).project;
                log_project_info(*dep, p.project->origin);
            } else {
                log.error("Failed to resolve dependency " + dep_id + " for project " + p.project->name, p.project->name);
            }
        }
    }

    // register default project
    if(auto& dp = default_project){
        auto project = instantiate_project(dp.value(), projects_.tracked_files, log);
        log.file_context = project->origin;
        log_project_info(*project, project->origin);

        for (auto& dep_id : dp->dependencies) {
            if(projects.contains(dep_id)) {
                auto& dep = projects.at(dep_id).project;
                project->dependencies.push_back(dep);            

                log_project_info(*dep, project->origin);
            } else {
                log.file_context = dp->origin;
                log.error("Failed to resolve dependency " + dep_id + " for default project " + project->name, dep_id);
            }
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
    for (auto* proj_file : project.collect_files()) {
        if(proj_file->path == file) return true;
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