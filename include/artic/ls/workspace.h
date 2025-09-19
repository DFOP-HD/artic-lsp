#ifndef ARTIC_LS_PROJECT_H
#define ARTIC_LS_PROJECT_H

#include <vector>
#include <string>
#include <optional>
#include <filesystem>
#include <unordered_map>

namespace artic::ls::workspace {

struct Project;
namespace config { struct ConfigLog; }

struct File {
    std::filesystem::path path;
    mutable std::optional<std::string> text; // TODO make non-mutable

    void read() const;

    explicit File(std::filesystem::path path) 
        : path(std::move(path)), text(std::nullopt) 
    {}
};

struct Project {
    using Identifier = std::string;
    Identifier name;
    std::filesystem::path origin; // config file that declared the project

    std::vector<std::shared_ptr<File>> files;
    std::vector<std::shared_ptr<Project>> dependencies;

    std::vector<const File*> collect_files() const;
    bool uses_file(const std::filesystem::path& file) const;
};

struct ProjectRegistry {
    std::shared_ptr<Project> default_project;
    std::vector<std::shared_ptr<Project>>   all_projects;
    std::unordered_map<std::filesystem::path, std::shared_ptr<File>> tracked_files;

    void print() const;
};

class Workspace {
public:
    Workspace(
        const std::filesystem::path& workspace_root,
        const std::filesystem::path& workspace_config_path = {},
        const std::filesystem::path& global_config_path = {})   
        : workspace_root(workspace_root), workspace_config_path(workspace_config_path), global_config_path(global_config_path)
    {}

    void reload(config::ConfigLog& log);
    std::optional<std::shared_ptr<Project>> project_for_file(const std::filesystem::path& file) const;
    std::shared_ptr<Project> default_project() const { return projects_.default_project; }
    
    ProjectRegistry projects_;
    std::optional<Project::Identifier> active_project;

    std::filesystem::path workspace_root;
    std::filesystem::path workspace_config_path;
    std::filesystem::path global_config_path;
};

} // namespace artic::ls

#endif // ARTIC_LS_PROJECT_H