#ifndef ARTIC_LS_SERVER_H
#define ARTIC_LS_SERVER_H

#include <memory>
#include <string>

#include "artic/ls/workspace.h"
#include "lsp/types.h"
#include <lsp/connection.h>
#include <lsp/messagehandler.h>
#include "artic/ls/compile.h"

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
    void compile_files(const std::vector<File>& files);

    lsp::Connection connection_;
    lsp::MessageHandler message_handler_;
private:
    bool running_;
    std::string workspace_root_;
    std::string workspace_config_path_;
    std::string global_config_path_;
    
    // Project management
    Workspace workspace_;
    std::unique_ptr<compiler::CompileResult> last_compilation_result_;

    void reload_workspace(const std::string& active_file = {});
    void publish_config_diagnostics();
};

} // namespace artic::ls

#endif // ARTIC_LS_SERVER_H