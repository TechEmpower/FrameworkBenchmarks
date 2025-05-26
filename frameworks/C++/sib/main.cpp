/*
  Mozilla Public License 2.0 (MPL-2.0)

  This file is part of the "https://github.com/PooyaEimandar/sib" project, which is licensed under
  the MPL-2.0. You can obtain a copy of the license at: https://opensource.org/licenses/MPL-2.0

  The MPL-2.0 allows you to use, modify, and distribute this file under certain conditions.
  The source code may be modified and distributed, provided that you retain the same license
  and include a copy of this notice when redistributing the code.

  This project was created by Pooya Eimandar.

  For more information on the terms of this license, please refer to the official MPL-2.0
  documentation.

  SPDX-License-Identifier: MPL-2.0
*/

#pragma once

#include <seastar/core/app-template.hh>
#include <seastar/http/httpd.hh>
#include <seastar/http/function_handlers.hh>
#include <seastar/core/sleep.hh>
#include <seastar/core/reactor.hh>
#include <iostream>

using namespace seastar;
using namespace seastar::httpd;

int main(int argc, char** argv) {
    app_template app;
    auto server = std::make_shared<http_server_control>();

    return app.run(argc, argv, [server] () -> future<> {
        constexpr auto port = 8080;
        constexpr auto server_name = "Sib HTTP Server";

        return server->start(server_name).then([server] {
            return server->set_routes([] (routes& p_routes) {
                p_routes.add(operation_type::GET, url("/plaintext"),
                    new function_handler(
                        [] (std::unique_ptr<http::request> p_req,
                            std::unique_ptr<http::reply> p_rep)
                        -> future<std::unique_ptr<http::reply>> {
                            p_rep->set_status(http::reply::status_type::ok);
                            p_rep->write_body("text/plain", "Hello, World!");
                            return make_ready_future<std::unique_ptr<http::reply>>(std::move(p_rep));
                        },
                        "text"
                    )
                );
            });
        }).then([server, port] {
            return server->listen(socket_address{ipv4_addr{port}});
        }).then([port]() -> future<> {
            std::cout << "Seastar HTTP server listening on port " << port << " ...\n";
            return sleep_abortable(std::chrono::hours(24)); 
        });
    });
}