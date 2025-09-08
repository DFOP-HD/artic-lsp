#ifndef ARTIC_LS_PROJECT_H
#define ARTIC_LS_PROJECT_H

#include <vector>
#include <string>
#include <optional>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <filesystem>

namespace artic::ls {

class File {
public:
    std::string path;
    std::optional<std::string> text;

    void read();

    explicit File(std::string path) 
        : path(std::move(path)), text(std::nullopt) 
    {}
};

struct ProjectEntry {
    std::string name;
    std::string root;                 // absolute directory
    std::vector<std::string> files;   // patterns or explicit paths; may contain leading ! for exclusion
    std::vector<std::string> dependencies;
    bool is_default = false;
};

struct ConfigIncludeRef {
    std::vector<std::string> projects; // empty => all
    std::string path;                  // path to another artic.json
    bool prefer_global = false;        // reserved
};

struct RawConfigDocument {
    std::string version;
    std::vector<ProjectEntry> projects;
    std::optional<ProjectEntry> default_project;
    std::vector<ConfigIncludeRef> includes;
};

struct ProjectFileResolutionResult {
    std::vector<std::string> files; // absolute, deduped
    std::vector<std::string> errors;
    std::vector<std::string> warnings;
};

class WorkspaceConfig {
public:
    struct LoadResult {
        std::vector<ProjectEntry> projects;
        std::vector<std::string> errors;
        std::vector<std::string> warnings;
    };

    static LoadResult load(std::string_view workspace_root,
                           const std::string& workspace_config_path,
                           const std::string& global_config_path);

    static ProjectFileResolutionResult resolve_files(const std::vector<ProjectEntry>& projects,
                                                     const std::string& active_file = "");
private:
    static std::optional<RawConfigDocument> parse_file(const std::filesystem::path& path,
                                                       std::vector<std::string>& errors,
                                                       std::vector<std::string>& warnings);
    static void collect_projects_recursive(const std::filesystem::path& path,
                                           bool is_global_root,
                                           std::unordered_set<std::string>& visited_configs,
                                           std::unordered_map<std::string, ProjectEntry>& out_projects,
                                           std::optional<ProjectEntry>& default_project,
                                           std::vector<std::string>& errors,
                                           std::vector<std::string>& warnings,
                                           const std::vector<std::string>* only_projects = nullptr);
    static void merge_project(ProjectEntry&& p,
                              std::unordered_map<std::string, ProjectEntry>& out_projects,
                              std::vector<std::string>& warnings);
};

class Workspace {
public:
    void load_from_config(std::string_view workspace_root,
                          const std::string& workspace_config_path = {},
                          const std::string& global_config_path = {},
                          const std::string& active_file = {});
    void handle_file_changed(std::string_view file_path);
    void handle_file_created(std::string_view file_path){/* TODO */}
    void handle_file_deleted(std::string_view file_path){/* TODO */}

    const std::vector<File>& get_project_files() const { return project_files_; }
    const std::vector<ProjectEntry>& project_definitions() const { return project_defs_; }
    const std::vector<std::string>& errors() const { return last_errors_; }
    const std::vector<std::string>& warnings() const { return last_warnings_; }

private:
    std::vector<File> project_files_;
    std::vector<ProjectEntry> project_defs_;
    std::vector<std::string> last_errors_;
    std::vector<std::string> last_warnings_;
};

} // namespace artic::ls

#endif // ARTIC_LS_PROJECT_H