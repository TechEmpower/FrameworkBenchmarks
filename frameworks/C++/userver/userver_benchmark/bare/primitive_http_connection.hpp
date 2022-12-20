#pragma once

#include <userver/engine/task/task_with_result.hpp>
#include <userver/engine/io/socket.hpp>

namespace userver_techempower::bare {

class PrimitiveHttpServer;

class PrimitiveHttpConnection final {
 public:
  explicit PrimitiveHttpConnection(PrimitiveHttpServer& server, userver::engine::io::Socket&& socket);
  ~PrimitiveHttpConnection();

 private:
  void Process();

  const PrimitiveHttpServer& server_;

  userver::engine::io::Socket socket_;

  userver::engine::TaskWithResult<void> processing_task_;
};

}  // namespace userver_techempower::bare