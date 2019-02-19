#include <drogon/drogon.h>
int main() {
    //Set HTTP listener address and port
    //app().setLogPath("./");
    //Print logs to standard output
    drogon::app().setLogLevel(trantor::Logger::WARN);
    drogon::app().addListener("0.0.0.0", 8080);
    drogon::app().setThreadNum(std::thread::hardware_concurrency());
    //app().enableRunAsDaemon();
    drogon::app().run();
    return 0;
}
