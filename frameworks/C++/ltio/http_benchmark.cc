#include <csignal>
#include <vector>

#include "base/utils/string/str_utils.h"
#include "fmt/chrono.h"
#include "fmt/format.h"
#include "gflags/gflags.h"
#include "net_io/server/http_server/http_server.h"
#include "nlohmann/json.hpp"

using namespace lt::net;
using namespace lt;
using namespace base;
using namespace co;

DEFINE_int32(loops, 4, "how many loops use for handle message and io");
DEFINE_bool(echo, false, "just echo response without any logic");
DEFINE_bool(coro, false, "using coro context handle request");
DEFINE_string(http,
              "0.0.0.0:5006",
              "host:port used for http service listen on");

class HttpBenchServer {
public:
  ~HttpBenchServer() {
    LOG(INFO) << __func__ << " close app";
    for (auto loop : loops) {
      delete loop;
    }
    loops.clear();
  }

  void Run() {
    json_message["message"] = "Hello, World!";

    int loop_count =
        std::max(FLAGS_loops, int(std::thread::hardware_concurrency()));
    LOG(INFO) << __func__ << " use loop count:" << loop_count;

    for (int i = 0; i < loop_count; i++) {
      loops.push_back(new base::MessageLoop(fmt::format("io_{}", i)));
      loops.back()->Start();
      CoroRunner::RegisteRunner(loops.back());
    }

    auto func = [this](const RefHttpRequestCtx& context) {
      const HttpRequest* req = context->Request();
      // TODO: response freelist
      auto response = HttpResponse::CreateWithCode(200);
      response->SetKeepAlive(req->IsKeepAlive());
      if (FLAGS_echo) {
        response->MutableBody() = "echo";
      } else {
        response->InsertHeader("Server", "ltio");
        auto tm = fmt::gmtime(std::time(nullptr));
        response->InsertHeader("Date",
                               fmt::format("{:%a, %d %b %Y %H:%M:%S %Z}", tm));
        if (req->RequestUrl() == "/plaintext") {
          response->MutableBody() = "Hello, World!";
        } else if (req->RequestUrl() == "/json") {
          response->InsertHeader("Content-Type", "application/json");
          response->MutableBody() = std::move(json_message.dump());
        }
      }
      return context->Response(response);
    };

    handler.reset(FLAGS_coro ? NewHttpCoroHandler(func) : NewHttpHandler(func));

    // ProfilerStart("perf.out");
    http_server.WithIOLoops(loops)
        .WithAddress(base::StrUtil::Concat("http://", FLAGS_http))
        .ServeAddress(handler.get());
    loops.back()->WaitLoopEnd();
  }

  void Stop() {
    CHECK(CO_CANYIELD);
    LOG(INFO) << __FUNCTION__ << " stop enter";
    http_server.StopServer(CO_RESUMER);
    CO_YIELD;
    LOG(INFO) << __FUNCTION__ << " stop leave";
    loops.back()->QuitLoop();
  }

  HttpServer http_server;
  std::unique_ptr<CodecService::Handler> handler;
  base::MessageLoop main_loop;
  nlohmann::json json_message;
  std::vector<base::MessageLoop*> loops;
};

HttpBenchServer app;

void signalHandler(int signum) {
  LOG(INFO) << "sighandler sig:" << signum;
  CO_GO std::bind(&HttpBenchServer::Stop, &app);
}

int main(int argc, char* argv[]) {
  gflags::ParseCommandLineFlags(&argc, &argv, true);
  gflags::SetUsageMessage("usage: exec --http=ip:port ");

  // google::InitGoogleLogging(argv[0]);
  // google::SetVLOGLevel(NULL, 26);

  signal(SIGINT, signalHandler);
  signal(SIGTERM, signalHandler);

  app.Run();
}
