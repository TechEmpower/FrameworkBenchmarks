#include <cuehttp.hpp>

using namespace cue::http;

int main(int argc, char** argv) {
    router route;
    route.post("/plaintext", [](context& ctx) {
        ctx.type("text/plain");
        ctx.status(200);
        ctx.body("Hello, World!");
    });
    cuehttp app;
    app.use(route);
    app.listen(8080).run();

    return 0;
}
