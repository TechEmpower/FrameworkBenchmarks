#include <httpserver.hpp>
#include <cstdlib>

#define PATH "/plaintext"
#define BODY "Hello, World!"

using namespace httpserver;

class hello_world_resource : public http_resource {
	public:
        const http_response render(const http_request&);
};

const http_response hello_world_resource::render(const http_request& req)
{
    http_response_builder hrb(BODY, 200);
    hrb.with_header("Server", "libhttpserver");
    return hrb.string_response();
}

int main(int argc, char** argv)
{
    webserver ws = create_webserver(atoi(argv[1]))
        .start_method(http::http_utils::INTERNAL_SELECT)
        .max_threads(atoi(argv[2]));

    hello_world_resource hwr;
    ws.register_resource(PATH, &hwr, false);

    ws.start(true);

    return 0;
}
