/// An example "HTTP server" with poor usability but sensible performance
///
module http.Processor;

import std.conv;
import std.array, std.exception, std.format, std.algorithm.mutation, std.socket;
import core.stdc.stdlib;
import core.thread, core.atomic;
import http.Parser;

import hunt.collection.ByteBuffer;
import http.Common;
import hunt.logging;
import hunt.io;
import hunt.util.DateTime;


private	alias Parser = HttpParser!HttpProcessor;


struct HttpRequest {
	private Parser* parser;

	HttpHeader[] headers(bool canCopy=false)() @property {
		return parser.headers!canCopy();
	}

	HttpMethod method() @property {
		return parser.method();
	}

	string uri(bool canCopy=false)() @property {
		return parser.uri!(canCopy)();
	}
}

version(NO_HTTPPARSER) {
enum string ResponseData = "HTTP/1.1 200 OK\r\nContent-Length: 13\r\nConnection: Keep-Alive\r\nContent-Type: text/plain\r\nServer: Hunt/1.0\r\nDate: Wed, 17 Apr 2013 12:00:00 GMT\r\n\r\nHello, World!";
}

abstract class HttpProcessor {
	
package:
	Appender!(char[]) outBuf;
	HttpHeader[] headers; // buffer for headers
	Parser parser;
	HttpRequest request;
	bool serving;
	
public:
	TcpStream client;

	this(TcpStream sock) {
		serving = true;
		client = sock;
		headers = new HttpHeader[1];
		parser = httpParser(this);
		request.parser = &parser;
	}

	void run() {
		client.onReceived(delegate int (ubyte[] buffer) { 
			version(NO_HTTPPARSER) {
				client.write(cast(ubyte[])ResponseData);
			} else {
				int len = 0;
				try {
					len = parser.execute(buffer);
				} catch(Exception ex) {
					respondWith(ex.msg, 500);
					len = cast(int)buffer.length;
				}

				return len;
			}
		})
		.onClosed(() {
			// notifyClientClosed();
		})
		.onError((string msg) { 
			debug warning("Error: ", msg); 
		})
		.start();
	}

	protected void notifyClientClosed() {
		debug tracef("The connection[%s] is closed", client.remoteAddress());
	}

	void respondWith(string _body, uint status, HttpHeader[] headers...) {
		return respondWith(cast(const(ubyte)[]) _body, status, headers);
	}

	void respondWith(const(ubyte)[] _body, uint status, HttpHeader[] headers...) {
		outBuf.clear();
		formattedWrite(outBuf, "HTTP/1.1 %s OK\r\n", status);
		outBuf.put("Server: Hunt/1.0\r\n");

		formattedWrite(outBuf, "Date: %s\r\n", DateTimeHelper.getDateAsGMT());
		if (!parser.shouldKeepAlive)
			outBuf.put("Connection: close\r\n");
		foreach (ref hdr; headers) {
			outBuf.put(hdr.name);
			outBuf.put(": ");
			outBuf.put(hdr.value);
			outBuf.put("\r\n");
		}
		formattedWrite(outBuf, "Content-Length: %d\r\n\r\n", _body.length);
		outBuf.put(cast(string) _body);
		client.write(cast(ubyte[]) outBuf.data); // TODO: short-writes are quite possible
	}

	void onChunk(ref HttpRequest req, const(ubyte)[] chunk) {
		// TODO: Tasks pending completion - 5/16/2019, 5:40:18 PM
		// 
	}

	void onComplete(ref HttpRequest req);


	final int onBody(Parser* parser, const(ubyte)[] chunk) {
		onChunk(request, chunk);
		return 0;
	}

	final int onMessageComplete() {
		try {
			onComplete(request);
		} catch(Exception ex) {
			respondWith(ex.msg, 500);
		}
		if (!parser.shouldKeepAlive)
			serving = false;
		return 0;
	}

}
