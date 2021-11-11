/*
 * Copyright (c) 2020, RTBtech, MediaSniper, Oleg Romanenko (oleg@romanenko.ro)
 */

#include <sniper/event/Loop.h>
#include <sniper/event/Timer.h>
#include <sniper/http/Server.h>
#include <sniper/log/log.h>
#include <sniper/std/check.h>
#include <sniper/std/vector.h>
#include <sniper/threads/Stop.h>
#include <thread>

using namespace sniper;

class Server
{
public:
    explicit Server()
    {
        for (unsigned i = 0; i < std::thread::hardware_concurrency(); i++)
            _workers.emplace_back(&Server::worker_noexcept, this, i);
    }

    ~Server()
    {
        for (auto& w : _workers)
            if (w.joinable())
                w.join();
    }

private:
    void worker_noexcept(unsigned int thread_num) noexcept
    {
        try {
            worker(thread_num);
        }
        catch (std::exception& e) {
            log_err(e.what());
        }
        catch (...) {
            log_err("[Server] non std::exception occured");
        }
    }

    void worker(unsigned thread_num)
    {
        auto loop = event::make_loop();
        check(loop, "[Server] cannot init event loop");

        auto config = http::server::make_config();
        config->add_server_header = true;
        config->add_date_header = true;
        config->max_conns = 100000;
        config->keep_alive_timeout = 0ms;
        config->buffer_size = 128 * 1024;

        http::Server http_server(loop, config);
        check(http_server.bind("", 8090), "[Server] cannot bind to localhost:8090");

        http_server.set_cb([&, this](const auto& conn, const auto& req, const auto& resp) {
            if (req->path() == "/plaintext") {
                resp->code = http::ResponseStatus::OK;
                resp->add_header_nocopy("Content-Type: text/plain; charset=UTF-8\r\n");
                resp->set_data_nocopy("Hello, World!");
                conn->send(resp);
                return;
            }

            resp->code = http::ResponseStatus::NO_CONTENT;
            conn->send(resp);
        });

        event::TimerRepeat timer_stop(loop, 1s, [&loop] {
            if (threads::Stop::get().is_stopped())
                loop->break_loop(ev::ALL);
        });

        loop->run();
    }

    vector<std::thread> _workers;
};

int main(int argc, char** argv)
{
    auto loop = event::make_loop();
    if (!loop) {
        log_err("Main: cannot init event loop");
        return EXIT_FAILURE;
    }

    signal(SIGPIPE, SIG_IGN);

    Server server;
    loop->run();

    return EXIT_SUCCESS;
}
