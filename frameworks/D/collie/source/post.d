module post;


import collie.codec.http;
import collie.codec.http.server;
import request;
import std.json;
import std.typecons;
import std.exception;
import kiss.container.Vector;

final class PostHandler : BaseHandler
{
	override void onEOM() nothrow
	{
		collectException({
				switch(_header.getPath)
				{
					case "/json":
						json();
						break;
					case "/plaintext":
						plaintext();
						break;
					default:
						index();
						break;
				}
			}());
	}
	
	void index()
	{
		auto build = scoped!ResponseBuilder(_downstream);
		build.status(200,HTTPMessage.statusText(200));
		build.setBody(cast(ubyte[])"Hello, World!");
		build.header(HTTPHeaderCode.CONTENT_TYPE,"text/plain");
		build.header(HTTPHeaderCode.DATE, printDate);
		build.sendWithEOM();
	}
	
	void json()
	{
		JSONValue js;
		js["message"] = "Hello, World!";
		
		auto build = scoped!ResponseBuilder(_downstream);
		build.status(200,HTTPMessage.statusText(200));
		build.setBody(cast(ubyte[])(js.toString));
		build.header(HTTPHeaderCode.CONTENT_TYPE,"application/json");
		build.header(HTTPHeaderCode.DATE, printDate);
		build.sendWithEOM();
	}
	
	void plaintext()
	{
		auto build = scoped!ResponseBuilder(_downstream);
		build.status(200,HTTPMessage.statusText(200));
		build.setBody(cast(ubyte[])"Hello, World!");
		build.header(HTTPHeaderCode.CONTENT_TYPE,"text/plain");
		build.header(HTTPHeaderCode.DATE, printDate);
		build.sendWithEOM();
	}
}

