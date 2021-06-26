#include <vector>
#include <csignal>

#include "fmt/chrono.h"
#include "gflags/gflags.h"
#include "base/utils/string/str_utils.h"
#include "net_io/server/http_server/http_server.h"


using namespace lt::net;
using namespace lt;
using namespace base;

DEFINE_int32(loops, 4, "how many loops use for handle message and io");
DEFINE_string(http, "0.0.0.0:5006", "host:port used for http service listen on");

class HttpBenchMarkServer {
public:
  ~HttpBenchMarkServer() {
    LOG(INFO) << __func__ << " close app";
    for (auto loop : loops) {
      delete loop;
    }
    loops.clear();
  }

  void Run() {
    main_loop.Start();

    int loop_count = std::max(FLAGS_loops, int(std::thread::hardware_concurrency()));
    LOG(INFO) << __func__ << " use loop count:" << loop_count;

    for (int i = 0; i < loop_count; i++) {
      auto loop = new(base::MessageLoop);
      loop->Start();
      loops.push_back(loop);
    }

    http_server.WithIOLoops(loops)
      .WithAddress(base::StrUtil::Concat("http://", FLAGS_http))
      .ServeAddress([](const RefHttpRequestCtx& context) {
        VLOG(GLOG_VTRACE) << " got Http request from benchmark api";

        const HttpRequest* req = context->Request();
        //TODO: response freelist
        auto response = HttpResponse::CreateWithCode(200);
        response->SetKeepAlive(req->IsKeepAlive());

        response->InsertHeader("Server", "ltio");
        auto tm = fmt::gmtime(std::time(nullptr));
        response->InsertHeader("Date", fmt::format("{:%a, %d %b %Y %H:%M:%S %Z}", tm));
        if (req->RequestUrl() == "/plaintext") {
          response->MutableBody() = "Hello, World!";
        } else if (req->RequestUrl() == "/json") {
          response->InsertHeader("Content-Type", "application/json");
          response->MutableBody() = "{\"message\":\"Hello, World!\"}";
        }
        return context->Response(response);
      });

    main_loop.WaitLoopEnd();
  }

  void Stop() {
    CHECK(CO_CANYIELD);
    LOG(INFO) << __FUNCTION__ << " stop enter";
    http_server.StopServer(CO_RESUMER);
    CO_YIELD;
    LOG(INFO) << __FUNCTION__ << " stop leave";
    main_loop.QuitLoop();
  }

  //HttpServer http_server;
  HttpCoroServer http_server;
  base::MessageLoop main_loop;
  std::vector<base::MessageLoop*> loops;
};

HttpBenchMarkServer app;

void signalHandler( int signum ){
  LOG(INFO) << "sighandler sig:" << signum;
  CO_GO std::bind(&HttpBenchMarkServer::Stop, &app);
}

int main(int argc, char* argv[]) {
  gflags::ParseCommandLineFlags(&argc, &argv, true);
  gflags::SetUsageMessage("usage: exec --http=ip:port ");

  //google::InitGoogleLogging(argv[0]);
  //google::SetVLOGLevel(NULL, 26);

  signal(SIGINT, signalHandler);
  signal(SIGTERM, signalHandler);

  app.Run();
}

