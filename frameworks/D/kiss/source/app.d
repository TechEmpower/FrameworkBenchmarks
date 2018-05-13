/*
 * Collie - An asynchronous event-driven network framework using Dlang development
 *
 * Copyright (C) 2015-2018  Shanghai Putao Technology Co., Ltd 
 *
 * Developer: putao's Dlang team
 *
 * Licensed under the Apache-2.0 License.
 *
 */
import std.stdio;

import kiss.event;
import kiss.net;
import kiss.util.thread;

import std.array;
import std.conv;
import std.json;
import std.functional;
import std.getopt;
import std.exception;
import std.datetime;
import std.parallelism;
import std.socket;
import std.string;

/**
*/
abstract class AbstractTcpServer
{
	protected EventLoopGroup _group = null;
	protected bool _isStarted = false;
	protected Address _address;

	this(Address address, int thread = (totalCPUs - 1))
	{
		this._address = address;
		_group = new EventLoopGroup(cast(uint) thread);
	}

	@property Address bindingAddress()
	{
		return _address;
	}

	void start()
	{
		if (_isStarted)
			return;

		for (size_t i = 0; i < _group.length; ++i)
			createServer(_group[i]);
		_group.start();
		_isStarted = true;
	}

	protected void createServer(EventLoop loop)
	{
		TcpListener listener = new TcpListener(loop, _address.addressFamily);

		listener.reusePort(true);
		listener.bind(_address).listen(1024);
		listener.acceptHandler = &onConnectionAccepted;
		listener.start();
	}

	protected void onConnectionAccepted(TcpListener sender, TcpStream client);

	void stop()
	{
		if (!_isStarted)
			return;
		_isStarted = false;
		_group.stop();
	}
}

/**
*/
class HttpServer : AbstractTcpServer
{
	this(string ip, ushort port, int thread = (totalCPUs - 1))
	{
		super(new InternetAddress(ip, port), thread);
	}

	this(Address address, int thread = (totalCPUs - 1))
	{
		super(address, thread);
	}

	override protected void onConnectionAccepted(TcpListener sender, TcpStream client)
	{
		client.onDataReceived((in ubyte[] data) {
			handleReceivedData(client, data);
		}).onClosed(() { notifyClientClosed(client); }).onError((string msg) {
			writeln("Error: ", msg);
		});
	}

	protected void handleReceivedData(TcpStream client, in ubyte[] data)
	{
		string request = cast(string) data;
		bool keepAlive = indexOf(request, " keep-alive", CaseSensitive.no) > 0;

		ptrdiff_t index = indexOf(request, "/plaintext ", CaseSensitive.no);
		if (index > 0)
			respondPlaintext(client, keepAlive);
		else if (indexOf(request, "/json ", CaseSensitive.no) > 0)
		{
			respondJson(client, keepAlive);
		}
		else
		{
			badRequest(client);
		}

	}

	private void respondPlaintext(TcpStream client, bool keepAlive)
	{
		string content = "Hello, World!";
		Appender!string sb;
		sb.put("HTTP/1.1 200 OK");
		sb.put("\r\n");
		sb.put("Server: Kiss/0.3");
		sb.put("\r\n");
		sb.put("Connection: Keep-Alive");
		sb.put("\r\n");
		sb.put("Content-Type: text/plain");
		sb.put("\r\n");
		sb.put("Content-Length: " ~ to!string(content.length));
		sb.put("\r\n");
		sb.put("Date: " ~ Clock.currTime.toString());
		sb.put("\r\n\r\n");
		sb.put(content);

		client.write(cast(ubyte[]) sb.data, (in ubyte[], size_t) {
			if (!keepAlive)
				client.close();
		});
	}

	private void respondJson(TcpStream client, bool keepAlive)
	{
		JSONValue js;
		js["message"] = "Hello, World!";
		string content = js.toString();

		Appender!string sb;
		sb.put("HTTP/1.1 200 OK");
		sb.put("\r\n");
		sb.put("Server: Kiss/0.3");
		sb.put("\r\n");
		sb.put("Connection: Keep-Alive");
		sb.put("\r\n");
		sb.put("Content-Type: application/json");
		sb.put("\r\n");
		sb.put("Content-Length: " ~ to!string(content.length));
		sb.put("\r\n");
		sb.put("Date: " ~ Clock.currTime.toString());
		sb.put("\r\n\r\n");
		sb.put(content);

		client.write(cast(ubyte[]) sb.data, (in ubyte[], size_t) {
			if (!keepAlive)
				client.close();
		});
	}

	private void badRequest(TcpStream client)
	{
		string content = `<html>
<head><title>404 Not Found</title></head>
<body bgcolor="white">
<center><h1>404 Not Found</h1></center>
<hr><center>Kiss/0.3</center>
</body>
</html>`;
		Appender!string sb;
		sb.put("HTTP/1.1 404 Not Found");
		sb.put("\r\n");
		sb.put("Server: Kiss/0.3");
		sb.put("\r\n");
		sb.put("Connection: keep-alive");
		sb.put("\r\n");
		sb.put("Content-Type: text/html");
		sb.put("\r\n");
		sb.put("Content-Length: " ~ to!string(content.length));
		sb.put("\r\n");
		sb.put("Date: " ~ Clock.currTime.toString());
		sb.put("\r\n\r\n");
		sb.put(content);

		client.write(cast(ubyte[]) sb.data);
		client.close();
	}

	protected void notifyClientClosed(TcpStream client)
	{
		debug writefln("The connection[%s] is closed on thread %s",
				client.remoteAddress(), getTid());
	}
}

void main(string[] args)
{
	ushort port = 8080;
	GetoptResult o = getopt(args, "port|p", "Port (default 8080)", &port);
	if (o.helpWanted)
	{
		defaultGetoptPrinter("A simple http server powered by KISS!", o.options);
		return;
	}

	HttpServer httpServer = new HttpServer("0.0.0.0", 8080, totalCPUs);
	writefln("listening on %s", httpServer.bindingAddress.toString());
	httpServer.start();
}
