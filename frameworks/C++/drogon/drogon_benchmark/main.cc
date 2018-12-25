#include <drogon/drogon.h>
int main() {
    //Set HTTP listener address and port
    //app().setLogPath("./");
    //Print logs to standard output
    app().setLogLevel(trantor::Logger::WARN);
    app().addListener("0.0.0.0", 8080);
    app().setThreadNum(std::thread::hardware_concurrency());
    //app().enableRunAsDaemon();
    app().run();
    return 0;
}
