using GLib;
using VSGI;

public class App : Handler
{
	public override bool handle (Request req, Response res) throws Error
	{
		res.headers.replace ("Server", "VSGI/0.3");
		switch (req.uri.path)
		{
			case "/json":
				res.headers.set_content_type ("application/json", null);
				var builder = new Json.Builder ();

				builder.begin_object ();
				builder.set_member_name ("message");
				builder.add_string_value ("Hello, World!");
				builder.end_object ();

				var gen = new Json.Generator ();
				gen.root = builder.get_root ();

				return res.expand (gen.to_data (null).data);
			case "/plaintext":
				res.headers.set_content_type ("text/plain", null);
				return res.expand ("Hello, World!".data);
			default:
				return false;
		}
	}
}

Server.new ("http", handler: new App ()).run ({"app", "--address=0.0.0.0:8080"});
