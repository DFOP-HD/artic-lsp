#ifndef ARTIC_LS_PROJECT_H
#define ARTIC_LS_PROJECT_H

#include <vector>
#include <string>
#include <optional>
#include <string_view>
#include <filesystem>

namespace artic::ls {

struct File {
    std::filesystem::path path;
    std::optional<std::string> text;

    void read();

    explicit File(std::filesystem::path path) 
        : path(std::move(path)), text(std::nullopt) 
    {}
};

struct Project {
    using Identifier = std::string;
    enum Origin { LocalDefinition, LocalInclude, GlobalDefinition, GlobalInclude };

    Identifier name;

    std::vector<File> files;
    std::vector<std::shared_ptr<Project>> dependencies;
};

struct WorkspaceConfig {
    std::optional<std::shared_ptr<Project>> default_project;
    std::vector<std::shared_ptr<Project>>   all_projects;

    struct Log {
        std::vector<std::string> errors;
        std::vector<std::string> warnings;
        void error(std::string msg) { errors.push_back(std::move(msg)); }
        void warn (std::string msg) { warnings.push_back(std::move(msg)); }
    } log;
};

class Workspace {
public:
    void load_from_config(const std::filesystem::path& workspace_root,
                          const std::filesystem::path& workspace_config_path = {},
                          const std::filesystem::path& global_config_path = {});

    void handle_file_changed(std::string_view file_path);
    void handle_file_created(std::string_view file_path){/* TODO */}
    void handle_file_deleted(std::string_view file_path){/* TODO */}

    std::vector<File*> get_project_files(const std::filesystem::path& active_file) const;

// private:
    WorkspaceConfig workspace_config_;
};


namespace config {


struct ProjectDefinition {
    // Unique project name.
    // May be referenced by other projects.
    Project::Identifier name;

    // Path to the project root directory.
    // FilePatterns are relative to this path.
    std::string root_dir;
    
    // A pattern which can be used to include or exclude one or more files.
    // Exclude patterns start with '!' character.
    std::vector<std::string> file_patterns;

    // Names of other projects that this project depends on.
    // Projects will include all files from dependencies.
    std::vector<Project::Identifier> dependencies;

    // config where project was first defined
    std::filesystem::path origin;
};

struct IncludeConfig {
    // path to another artic.json
    std::filesystem::path path; 
    bool is_optional = false;
};

struct ConfigDocument {
    std::string version;
    std::vector<ProjectDefinition>       projects;
    std::optional<ProjectDefinition>     default_project;
    std::vector<IncludeConfig> includes;
    std::filesystem::path                path;
};

} // namespace config

} // namespace artic::ls

#endif // ARTIC_LS_PROJECT_H