#include <iostream>
#include <luna/luna.h>
#include "common.h"

// Main entrypoint

int main(int argc, char **argv) {
    if (argc != 2) {
        std::cerr << "Usage: " << argv[0] << " port" << std::endl;
        return 1;
    }

    auto port = static_cast<uint16_t>(std::atoi(argv[1]));

    luna::server server{
            luna::server::port{port},
            luna::server::use_thread_per_connection{true},
    };

    server.handle_request(luna::request_method::GET,
                          "/plaintext",
                          plaintext_handler);

    server.handle_request(luna::request_method::GET,
                          "/json",
                          json_handler);

    server.await();
}
