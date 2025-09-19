#ifndef ARTIC_LS_PROJECT_H
#define ARTIC_LS_PROJECT_H

#include "lsp/types.h"
#include <vector>
#include <string>
#include <optional>
#include <filesystem>
#include <unordered_map>

namespace artic::ls::workspace {

struct Project;

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

struct ConfigLog;
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

    void reload(ConfigLog& log);

    std::optional<std::shared_ptr<Project>> project_for_file(const std::filesystem::path& file) const;
    std::shared_ptr<Project> default_project() const { return projects_.default_project; }
    
    std::optional<Project::Identifier> active_project;

    std::filesystem::path workspace_root;
    std::filesystem::path workspace_config_path;
    std::filesystem::path global_config_path;
    ProjectRegistry projects_;

private:
    bool is_file_part_of_project(const Project& project, const std::filesystem::path& file) const;
};

struct ConfigLog {
    using Severity = lsp::DiagnosticSeverity;
    struct Context {
        std::string literal;
    };
    struct Message {
        std::string message;
        Severity severity;

        std::filesystem::path file;
        std::optional<Context> context;
    };
    std::filesystem::path file_context;
    std::vector<Message> messages;

    void error(std::string msg, std::optional<std::string> context=std::nullopt) { messages.push_back(make_message(Severity::Error,       std::move(msg), context)); }
    void warn (std::string msg, std::optional<std::string> context=std::nullopt) { messages.push_back(make_message(Severity::Warning,     std::move(msg), context)); }
    void info (std::string msg, std::optional<std::string> context=std::nullopt) { messages.push_back(make_message(Severity::Information, std::move(msg), context)); }

private:
    static std::string quote(std::string_view in) {
        return '\"' + std::string(in) + '\"';
    }
    Message make_message(Severity s, std::string msg, std::optional<std::string> context) {
        return Message{
            .message=std::move(msg),
            .severity=s,
            .file=file_context, 
            .context= context 
                ? std::make_optional(Context{quote(context.value())}) 
                : std::nullopt
        };
    }
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

    int depth = 100;
};

struct IncludeConfig {
    // path to another artic.json
    std::filesystem::path path;
    std::string raw_path_string;
    bool is_optional = false;
};

struct ConfigDocument {
    std::string version;
    std::vector<ProjectDefinition>   projects;
    std::optional<ProjectDefinition> default_project;
    std::vector<IncludeConfig>       includes;
    std::filesystem::path            path;
    
    static std::optional<ConfigDocument> parse(const IncludeConfig& config, ConfigLog& log);
};

} // namespace config

} // namespace artic::ls

#endif // ARTIC_LS_PROJECT_H