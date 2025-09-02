#ifndef ARTIC_LS_PROJECT_H
#define ARTIC_LS_PROJECT_H

#include <vector>
#include <string>
#include <optional>
#include <string_view>

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

class WorkspaceConfig {
public:
    static std::optional<std::vector<std::string>> evaluate_config_file(std::string_view workspace_root);

    std::vector<std::string> include_patterns;
    std::vector<std::string> exclude_patterns;
    std::vector<std::string> explicit_files;
    std::string workspace_root;
private:
    WorkspaceConfig() = default;
    std::vector<std::string> get_all_included_files() const;
    bool should_exclude_file(std::string_view file_path) const;
    std::vector<std::string> expand_include_patterns() const;
};

class Workspace {
public:
    void load_from_config(std::string_view workspace_root);
    void handle_file_changed(std::string_view file_path);
    void handle_file_created(std::string_view file_path){/* TODO */}
    void handle_file_deleted(std::string_view file_path){/* TODO */}

    const std::vector<File>& get_project_files() const { return project_files_; }

private:
    std::vector<File> project_files_;
};

} // namespace artic::ls

#endif // ARTIC_LS_PROJECT_H