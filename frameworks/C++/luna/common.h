//
// Created by Don Goodman-Wilson on 15/04/2017.
//

#pragma once

#include <luna/luna.h>
#include <nlohmann/json.hpp>

using json = nlohmann::json;

// Request handlers

// /json
auto json_handler = [](auto req) -> luna::response {
    json j;
    j["message"] = "Hello, World!";

    return {
            200,
            luna::response_headers{{"Server", "luna"}},
            "application/json",
            j.dump(),
    };
};

// /plaintext
auto plaintext_handler = [](auto req) -> luna::response {
    return {
            200,
            luna::response_headers{{"Server", "luna"}},
            "text/plain",
            "Hello, world!",
    };
};

