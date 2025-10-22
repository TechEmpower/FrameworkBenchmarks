#pragma once

#include <userver/engine/io/socket.hpp>
#include <userver/engine/task/task_with_result.hpp>

namespace userver_techempower::bare {

class SimpleServer;

class SimpleConnection final {
 public:
  explicit SimpleConnection(SimpleServer& server,
                            userver::engine::io::Socket&& socket);
  ~SimpleConnection();

 private:
  void Process();

  const SimpleServer& server_;

  userver::engine::io::Socket socket_;

  userver::engine::TaskWithResult<void> processing_task_;
};

}  // namespace userver_techempower::bare
