#ifndef ARTIC_LS_SERVER_H
#define ARTIC_LS_SERVER_H

#include <memory>
#include <string>

#include "artic/ls/workspace.h"
#include "lsp/types.h"
#include <lsp/connection.h>
#include <lsp/messagehandler.h>
#include "artic/ls/compile.h"
#include <span>

namespace artic::ls {

/**
 * Minimal LSP server implementation for Artic language support.
 * Uses basic JSON-RPC over stdio communication.
 */
class Server {
public:
    Server();
    ~Server();

    /// Start the LSP server main loop
    int run();
    void setup_events();
    void send_message(const std::string& message, lsp::MessageType type);
    void compile_files(std::span<const workspace::File*> files);
    void compile_file(const std::filesystem::path& file);

    enum class FileType { SourceFile, ConfigFile };
    static FileType get_file_type(const std::filesystem::path& file);

    lsp::Connection connection_;
    lsp::MessageHandler message_handler_;
    bool running_;
    
    // Project management
    std::unique_ptr<workspace::Workspace> workspace_;
    std::unique_ptr<compiler::CompileResult> last_compile;

    void reload_workspace(const std::string& active_file = {});
    void publish_config_diagnostics(const workspace::config::ConfigLog& log);
    void ensure_compile(std::string_view file_view);
};

} // namespace artic::ls

#endif // ARTIC_LS_SERVER_H