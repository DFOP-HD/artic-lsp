#include "artic/ls/workspace.h"
#include "artic/log.h"

#include <algorithm>
#include <fstream>
#include <filesystem>
#include <fnmatch.h>
#include <iostream>
#include <vector>

#ifdef ENABLE_JSON
#include <nlohmann/json.hpp>
#endif

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

void Workspace::load_from_config(std::string_view workspace_root) {
    // Load workspace config
    auto file_list = WorkspaceConfig::evaluate_config_file(workspace_root);
    if (file_list) {
        project_files_.clear();
        for (const auto& file_path : *file_list) {
            project_files_.emplace_back(file_path);
        }
        log::debug("Loaded {} files into workspace.", project_files_.size());
    } else {
        log::debug("Could not find workspace configuration. Please create artic.json in your root workspace directory and restart the language server.");
    }

    // Read file contents
    for (auto& file : project_files_) {
        file.read();
        log::debug("Read file '{}'", file.path);
    }
}

// WorkspaceConfig ----------------------------------------------------------------------

std::optional<std::vector<std::string>> WorkspaceConfig::evaluate_config_file(std::string_view workspace_root) {
    static_assert(ENABLE_JSON, "JSON support not enabled, cannot parse workspace config (artic.json)");

    std::string config_path = std::string(workspace_root) + "/artic.json";

    if (!std::filesystem::exists(config_path)) {
        // No config file, use defaults
        return {};
    }

    WorkspaceConfig config;

    try {
        std::ifstream file(config_path);
        nlohmann::json json;
        file >> json;
        
        if (json.contains("include")) {
            config.include_patterns = json["include"].get<std::vector<std::string>>();
        }
        
        if (json.contains("exclude")) {
            config.exclude_patterns = json["exclude"].get<std::vector<std::string>>();
        }
        
        if (json.contains("files")) {
            config.explicit_files = json["files"].get<std::vector<std::string>>();
        }
        
        config.workspace_root = workspace_root;
        
        std::clog << "Loaded project configuration from " << config_path << std::endl;
        std::clog << "  Include patterns: " << config.include_patterns.size() << std::endl;
        std::clog << "  Exclude patterns: " << config.exclude_patterns.size() << std::endl;
        std::clog << "  Explicit files: " << config.explicit_files.size() << std::endl;
        
    } catch (const std::exception& e) {
        std::cerr << "Error parsing " << config_path << ": " << e.what() << std::endl;
        return {};
    }

    return config.get_all_included_files();
}
    
std::vector<std::string> WorkspaceConfig::get_all_included_files() const {
    std::vector<std::string> files;
    
    // Add explicit files first
    for (const auto& file : explicit_files) {
        std::string full_path;
        if (std::filesystem::path(file).is_absolute()) {
            full_path = file;
        } else {
            full_path = workspace_root + "/" + file;
        }
        files.push_back(full_path);
    }
    
    // Add files from include patterns
    auto pattern_files = expand_include_patterns();
    files.insert(files.end(), pattern_files.begin(), pattern_files.end());


    auto end = files.end();
    std::sort(files.begin(), end);
    end = std::unique(files.begin(), end);
    end = std::remove_if(files.begin(), end, [this](const std::string& file) {
        return should_exclude_file(file);
    });
    files.erase( end, files.end());
    return files;
}

bool WorkspaceConfig::should_exclude_file(std::string_view file_path) const{
    for (const auto& pattern : exclude_patterns) {
        // Simple pattern matching - can be enhanced
        if (file_path.find(pattern) != std::string::npos) {
            return true;
        }
        
        // Also check filename against pattern
        std::filesystem::path path(file_path);
        if (fnmatch(pattern.c_str(), path.filename().c_str(), 0) == 0) {
            return true;
        }
    }
    return false;
}

std::vector<std::string> WorkspaceConfig::expand_include_patterns() const{
    std::vector<std::string> files;
        
    for (const auto& pattern : include_patterns) {
        // For other patterns, do a simple directory scan with fnmatch
        try {
            std::filesystem::path pattern_path(pattern);
            std::string dir = workspace_root;
            std::string file_pattern = pattern;
            
            if (pattern_path.has_parent_path()) {
                dir += "/" + pattern_path.parent_path().string();
                file_pattern = pattern_path.filename().string();
            }
            
            if (std::filesystem::exists(dir)) {
                for (const auto& entry : std::filesystem::directory_iterator(dir)) {
                    if (entry.is_regular_file()) {
                        if (fnmatch(file_pattern.c_str(), entry.path().filename().c_str(), 0) == 0) {
                            files.push_back(entry.path().string());
                        }
                    }
                }
            }
        } catch (const std::exception& e) {
            std::cerr << "Error processing pattern " << pattern << ": " << e.what() << std::endl;
        }
    }
    
    return files;
}

} // namespace artic::ls