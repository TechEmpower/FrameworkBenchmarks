module app;

import std.json;
import serverino;
import std.parallelism: totalCPUs;

mixin ServerinoMain;

void catchAll(Request request, Output output)
{
	if (request.path == "/json") {

		output ~= JSONValue(["message" : "Hello, World!"]).toString();
		output.addHeader("content-type", "application/json");
	}
	else if (request.path == "/plaintext") {
		output ~= "Hello, World!";
		output.addHeader("content-type", "text/plain");
	}
	else output.status = 200;
}


@onServerInit ServerinoConfig configure()
{
	return ServerinoConfig
		.create()
		.enableServerSignature(true)
		.setWorkers(4)
		.setDaemonThreads(totalCPUs)
		.addListener("0.0.0.0", 8080);
}


