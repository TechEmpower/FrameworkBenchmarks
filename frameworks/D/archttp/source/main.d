
import archttp;

void main()
{
    auto app = new Archttp;

    app.get("/textplain", (req, res) {
        res.send("Hello, World!");
    });

    app.get("/json", (req, res) {
        import std.json;

        res.send( JSONValue( ["message" : "Hello, World!"] ) );
    });

    app.listen(1111);
}
