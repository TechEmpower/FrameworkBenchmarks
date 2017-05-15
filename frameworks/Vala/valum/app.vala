using GLib;
using Valum;
using Valum.ContentNegotiation;
using VSGI;

int main (string[] args)
{
	var app = new Router ();

	app.use ((req, res, next) => {
		res.headers.replace ("Server", "VSGI/0.3");
		return next ();
	});

	app.get ("/plaintext", accept ("text/plain", (req, res) => {
		return res.expand ("Hello, World!".data);
	}));

	app.get ("/json", accept ("application/json", (req, res, next, stack) => {
		var builder = new Json.Builder ();

		builder.begin_object ();
		builder.set_member_name ("message");
		builder.add_string_value ("Hello, World!");
		builder.end_object ();

		var gen = new Json.Generator ();
		gen.root = builder.get_root ();

		return res.expand (gen.to_data (null).data);
	}));

	return Server.@new ("http", handler: app).run (args);
}
