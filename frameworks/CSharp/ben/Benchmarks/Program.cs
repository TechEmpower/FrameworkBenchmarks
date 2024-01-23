using System;
using Ben.Http;
using Microsoft.Net.Http.Headers;
using Sql = Npgsql.NpgsqlConnection;

var connection = Environment.GetEnvironmentVariable("DB_CONNECTION");
var (server, app) = (new HttpServer("http://+:8080"), new HttpApp());

app.Get("/plaintext", () => "Hello, World!");

app.Get("/json", (req, res) => {
    res.Headers.ContentLength = 27;
    return res.Json(new Note { message = "Hello, World!" });
});

app.Get("/fortunes", async (req, res) => {
    res.Headers[HeaderNames.ContentType] = "text/html; charset=UTF-8";
    using Sql db = new(connection);
    var model = await db.QueryAsync<(int id, string message)>("SELECT id, message FROM fortune");
    model.Add((0, "Additional fortune added at request time."));
    model.Sort((x, y) => string.CompareOrdinal(x.message, y.message));
    MustacheTemplates.RenderFortunes(model, res.Writer);
});

app.Get("/db", async (req, res) => {
    using Sql db = new(connection);
    await res.Json(await db.QueryRowAsync<World, int>(
        "SELECT id, randomnumber FROM world WHERE id = @id",
        (name: "@id", value: ConcurrentRandom.Next(10000) + 1)));
});

await server.RunAsync(app);

struct Note { public string message { get; set; } }
struct World { public int id { get; set; } public int randomnumber { get; set; } }
