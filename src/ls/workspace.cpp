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
    // TODO read the file
    // Note: this can be done way easier if we just get the message from the lsp as it sends the file content anyways
}

std::vector<File*> Workspace::get_project_files(const std::filesystem::path& active_file) const{
    // TODO look for most fitting project for active file in workspace_config_
    // When there is no active file, return default project files plus the active file
    return {};
}

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

void Workspace::load_from_config(
    const std::filesystem::path& workspace_root,
    const std::filesystem::path& workspace_config_path,
    const std::filesystem::path& global_config_path
) {
    workspace_config_ = WorkspaceConfig();

    if(auto global_config = config::parse_config(global_config_path, workspace_config_.log)) {
        // TODO recursive load of included configs
    }

    if(auto local_config = config::parse_config(workspace_config_path, workspace_config_.log)) {
        // TODO recursive load of included configs
    }

    // resolve projects, dependencies, and file lists
    // ...
}

} // namespace artic::ls