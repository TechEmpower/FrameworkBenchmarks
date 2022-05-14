
import archttp;

void main()
{
    auto app = new Archttp;

    app.Bind(1111);

    app.Get("/textplain", (context) {
        auto response = context.response();
        response.body("Hello, World!");
    });

    app.Get("/json", (context) {
        import std.json;

        auto response = context.response();
        auto j = JSONValue( ["message" : "Hello, World!"] );

        response.json(j);
    });

    app.Run();
}
