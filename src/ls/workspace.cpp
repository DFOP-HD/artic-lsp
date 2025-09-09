#include "artic/ls/workspace.h"
#include "artic/log.h"

#include <algorithm>
#include <fstream>
#include <filesystem>
#include <fnmatch.h>
#include <iostream>
#include <vector>
#include <unordered_map>
#include <unordered_set>

#include <nlohmann/json.hpp>

namespace artic::ls {

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

void File::read() {
    text = read_file(path);
    if (!text) {
        log::error("Could not read file {}", path);
    }
}

// Workspace ----------------------------------------------------------------------

void Workspace::handle_file_changed(std::string_view file_path){
    auto it = std::find_if(project_files_.begin(), project_files_.end(),
        [file_path](const File& f) { return f.path == file_path; });
    if (it != project_files_.end()) {
        it->read();
    } else {
        log::debug("Changed file not in workspace: {}", file_path);
    }
}


// static void collect_projects_recursive(WorkspaceConfig& out) {
//     auto norm = normalize_path(path);
//     if (visited_configs.count(norm)) {
//         warnings.push_back("Config include cycle detected: " + norm);
//         return;
//     }
//     visited_configs.insert(norm);

//     auto doc = parse_file(path, errors, warnings);
//     if (!doc) return;

//     auto base_dir = path.parent_path();

//     const char* home = std::getenv("HOME");

//     for (auto& p : doc->projects) {
//         if (only_projects && !only_projects->empty()) {
//             if (std::find(only_projects->begin(), only_projects->end(), p.name) == only_projects->end()) {
//                 continue; // skip unrequested project
//             }
//         }
//         if (p.root_dir.empty()) p.root_dir = base_dir.string();
//         else if (p.root_dir.starts_with("~")) {
//             if (home) p.root_dir = std::string(home) + p.root_dir.substr(1);
//             else errors.push_back("Cannot resolve home path '~': HOME env variable is not set");
//         } else if (!std::filesystem::path(p.root_dir).is_absolute()) {
//             p.root_dir = (base_dir / p.root_dir).string();
//         }
//         p.root_dir = normalize_path(p.root_dir);
//         merge_project(std::move(p), out_projects, warnings);
//     }
//     if (doc->default_project) {
//         auto dp = *doc->default_project;
//         if (dp.root_dir.empty()) dp.root_dir = base_dir.string();
//         dp.root_dir = normalize_path(dp.root_dir);
//         if (!default_project) default_project = std::move(dp);
//     }
//     for (auto& inc : doc->includes) {
//         if (inc.path.empty()) continue;
//         std::filesystem::path inc_path = inc.path;
//         if (inc_path.string().starts_with("~")) {
//             if (home) inc_path = std::string(home) + inc_path.string().substr(1);
//             else errors.push_back("Cannot resolve home path '~': HOME env variable is not set");
//         } else if (!inc_path.is_absolute()) {
//             inc_path = base_dir / inc_path;
//         }
//         const std::vector<std::string>* filter = nullptr;
//         if (!inc.projects.empty()) filter = &inc.projects; // restrict to listed projects only
//         collect_projects_recursive(inc_path, false, visited_configs, out_projects, default_project, errors, warnings, filter);
//     }
// }

namespace config {

static std::optional<ConfigDocument> parse_config(const std::filesystem::path& path, WorkspaceConfig::Log& log) {
    if (!std::filesystem::exists(path)) {
        log.error("Config file does not exist: " + path.string());
        return std::nullopt;
    }
    try {
        nlohmann::json j; 
        std::ifstream is(path);
        is >> j;

        ConfigDocument doc;
        // if (!j.contains("artic-config")) {
        //     log.error(
        //         "Missing artic-config header in " + path.string()
        //         + "\nExample: " + nlohmann::json{{"artic-config", "1.0"}}.dump()
        //     );
        //     return std::nullopt;
        // }
        doc.version = j["artic-config"].get<std::string>();
        if (doc.version != "1.0") {
            log.warn("Unsupported artic-config version in " + path.string());
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
        if (j.contains("include")) {
            for (auto& incj : j["include"]) {
                IncludeExternalProjects ref;
                ref.req_projects      = incj.value<std::vector<std::string>>("projects", {});
                ref.path          = incj.value<std::string>("path", "");
                ref.prefer_global = incj.value<bool>("prefer-global", false);
                doc.includes.push_back(std::move(ref));
            }
        }
        return doc;
    } catch (const std::exception& e) {
        log.error(std::string("Failed to parse ") + path.string() + ": " + e.what());
        return std::nullopt;
    }
}

} // config

std::filesystem::path to_absolute_path(const std::filesystem::path& base_dir, std::string_view path) {
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

void Workspace::load_from_config(
    const FilePath& workspace_root,
    const FilePath& workspace_config_path,
    const FilePath& global_config_path
) {
    workspace_config_ = WorkspaceConfig();
    std::unordered_map<Project::Identifier, config::ProjectDefinition> project_map;
    std::unordered_set<std::string> visited;

    if(auto global_config = config::parse_config(global_config_path, workspace_config_.log)) {
        for(auto& p : global_config->projects) {
            if(project_map.contains(p.name)) {
                workspace_config_.log.warn("Duplicate project definition for '" + p.name + "' ignored");
                continue;
            }
            project_map.emplace(p.name, std::move(p));
        }
        std::vector<config::IncludeExternalProjects> include_queue = std::move(global_config->includes);
        auto global_base_dir = std::filesystem::path(workspace_config_path).parent_path();
    }


    // project_files_.clear();
    // project_defs_.clear();
    // last_errors_.clear();
    // last_warnings_.clear();

    // auto load_result = WorkspaceConfig::load(workspace_root, workspace_config_path, global_config_path);
    // project_defs_ = load_result.projects;
    // last_errors_ = load_result.errors;
    // last_warnings_ = load_result.warnings;

    // auto resolved = WorkspaceConfig::resolve_files(project_defs_, active_file);
    // // append structural errors
    // last_errors_.insert(last_errors_.end(), resolved.errors.begin(), resolved.errors.end());
    // last_warnings_.insert(last_warnings_.end(), resolved.warnings.begin(), resolved.warnings.end());

    // for (auto& f : resolved.files) {
    //     File file{f};
    //     file.read();
    //     project_files_.push_back(std::move(file));
    // }
    log::debug("Workspace loaded with {} files, {} errors, {} warnings.", project_files_.size(), last_errors_.size(), last_warnings_.size());
}

// WorkspaceConfig ----------------------------------------------------------------------

// ---------------- New Config Parsing ----------------

namespace {
    std::string normalize_path(const std::filesystem::path& p) {
        try { return std::filesystem::weakly_canonical(p).string(); }
        catch(...) { return std::filesystem::absolute(p).string(); }
    }

    bool is_glob(const std::string& s) {
        return s.find('*') != std::string::npos || s.find('?') != std::string::npos;
    }

    bool match_pattern(const std::string& pattern, const std::filesystem::path& test) {
        auto fname = test.filename().string();
        return fnmatch(pattern.c_str(), fname.c_str(), 0) == 0;
    }
}

void WorkspaceConfig::merge_project(ProjectDefinition&& p,
                                    std::unordered_map<std::string, ProjectDefinition>& out_projects,
                                    std::vector<std::string>& warnings) {
    if (p.name.empty()) return;
    if (out_projects.find(p.name) != out_projects.end()) {
        warnings.push_back("Duplicate project definition for '" + p.name + "' ignored");
        return;
    }
    out_projects.emplace(p.name, std::move(p));
}

ConfigEvaluationResult WorkspaceConfig::resolve_files(const std::vector<ProjectDefinition>& projects,
                                                          const std::string& active_file) {
    ConfigEvaluationResult res;
    std::unordered_set<std::string> added;

    // Build name -> project map for dependency lookups
    std::unordered_map<std::string, const ProjectDefinition*> project_by_name;
    for (const auto& p : projects) {
        if (!p.name.empty()) project_by_name[p.name] = &p;
    }

    // Topological sort with cycle + missing dependency diagnostics
    enum class Mark { Temporary, Permanent };
    std::unordered_map<std::string, Mark> marks;
    std::vector<const ProjectDefinition*> ordered; ordered.reserve(projects.size());
    std::vector<std::string> stack; stack.reserve(16);

    std::function<void(const ProjectDefinition&)> visit = [&](const ProjectDefinition& proj) {
        if (proj.name.empty()) return; // unnamed shouldn't have deps
        auto itMark = marks.find(proj.name);
        if (itMark != marks.end()) {
            if (itMark->second == Mark::Temporary) {
                // Cycle detected: stack contains path to proj.name
                std::string cycle = proj.name;
                // Build human-readable cycle chain
                for (auto rit = stack.rbegin(); rit != stack.rend(); ++rit) {
                    if (*rit == proj.name) break;
                    cycle = *rit + " -> " + cycle;
                }
                res.warnings.push_back("Dependency cycle detected: " + cycle);
            }
            return; // already processed or currently processing
        }
        marks[proj.name] = Mark::Temporary;
        stack.push_back(proj.name);
        for (const auto& depName : proj.dependencies) {
            if (depName == proj.name) {
                res.warnings.push_back("Project '" + proj.name + "' lists itself as a dependency");
                continue;
            }
            auto itDep = project_by_name.find(depName);
            if (itDep == project_by_name.end()) {
                res.warnings.push_back("Unknown dependency '" + depName + "' referenced by project '" + proj.name + "'");
                continue;
            }
            visit(*itDep->second);
        }
        stack.pop_back();
        marks[proj.name] = Mark::Permanent;
        ordered.push_back(&proj);
    };

    // Visit in original order to preserve user intent; dependencies inserted before dependents
    for (const auto& p : projects) {
        if (!p.name.empty()) visit(p);
        else {
            // unnamed (default) projects may still have dependencies
            ordered.push_back(&p);
        }
    }

    auto process_project_files = [&](const ProjectDefinition& p){
        for (const auto& entry : p.files) {
            bool is_exclusion = (!entry.empty() && entry[0] == '!');
            std::string pattern = is_exclusion ? entry.substr(1) : entry;
            std::filesystem::path pattern_path(pattern);
            std::filesystem::path base_dir = p.root;
            if (pattern_path.is_absolute()) base_dir = pattern_path.parent_path();
            std::string file_pattern;
            if (pattern_path.is_absolute()) file_pattern = pattern_path.filename().string();
            else if (pattern_path.has_parent_path()) {
                base_dir = base_dir / pattern_path.parent_path();
                file_pattern = pattern_path.filename().string();
            } else {
                file_pattern = pattern_path.string();
            }
            if (is_glob(file_pattern)) {
                if (std::filesystem::exists(base_dir)) {
                    for (auto& dir_entry : std::filesystem::directory_iterator(base_dir)) {
                        if (!dir_entry.is_regular_file()) continue;
                        if (match_pattern(file_pattern, dir_entry.path())) {
                            auto norm = normalize_path(dir_entry.path());
                            if (is_exclusion) added.erase(norm); else added.insert(norm);
                        }
                    }
                }
            } else {
                std::filesystem::path candidate = pattern_path.is_absolute() ? pattern_path : (p.root / pattern_path);
                auto norm = normalize_path(candidate);
                if (is_exclusion) {
                    added.erase(norm);
                } else if (std::filesystem::exists(candidate)) {
                    added.insert(norm);
                } else {
                    res.warnings.push_back("File pattern did not match any file: " + candidate.string());
                }
            }
        }
    };

    for (const auto* projPtr : ordered) {
        if (projPtr) process_project_files(*projPtr);
    }

    if (!active_file.empty()) {
        try {
            auto norm = normalize_path(active_file);
            added.insert(norm);
        } catch(...) {}
    }
    for (auto& f : added) res.files.push_back(f);
    std::sort(res.files.begin(), res.files.end());
    return res;
}

} // namespace artic::ls