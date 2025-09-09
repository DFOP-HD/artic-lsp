#ifndef ARTIC_LS_PROJECT_H
#define ARTIC_LS_PROJECT_H

#include <vector>
#include <string>
#include <optional>
#include <string_view>
#include <filesystem>

namespace artic::ls {

using FilePath = std::filesystem::path;

struct File {
    FilePath path;
    std::optional<std::string> text;

    void read();

    explicit File(FilePath path) 
        : path(std::move(path)), text(std::nullopt) 
    {}
};

struct Project {
    using Identifier = std::string;
    enum Origin { LocalDefinition, LocalInclude, GlobalDefinition, GlobalInclude };

    Identifier name;
    Origin     origin;

    std::vector<File> files;
    std::vector<Project*> dependencies;
};

struct WorkspaceConfig {
    std::optional<Project> default_project;
    std::vector<Project>   projects;

    struct Log {
        std::vector<std::string> errors;
        std::vector<std::string> warnings;
        void error(std::string msg) { errors.push_back(std::move(msg)); }
        void warn (std::string msg) { warnings.push_back(std::move(msg)); }
    } log;
};

class Workspace {
public:
    void load_from_config(const FilePath& workspace_root,
                          const FilePath& workspace_config_path = {},
                          const FilePath& global_config_path = {});

    void handle_file_changed(std::string_view file_path);
    void handle_file_created(std::string_view file_path){/* TODO */}
    void handle_file_deleted(std::string_view file_path){/* TODO */}

    const std::vector<File>& get_project_files(const FilePath& active_file) const;

private:
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

    // The default project will be used a file is edited that does not belong to any project.
    bool is_default = false;
};

struct IncludeExternalProjects {
    std::vector<Project::Identifier> req_projects; // filter (empty => all)
    std::string path;            // path to another artic.json
    bool prefer_global = false;  // use project from global config if available
};

struct ConfigDocument {
    std::string version;
    std::vector<ProjectDefinition>       projects;
    std::optional<ProjectDefinition>     default_project;
    std::vector<IncludeExternalProjects> includes;
};

} // namespace config

} // namespace artic::ls

#endif // ARTIC_LS_PROJECT_H