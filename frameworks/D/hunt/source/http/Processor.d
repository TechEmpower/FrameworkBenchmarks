/// An example "HTTP server" with poor usability but sensible performance
///
module http.Processor;

import std.array, std.exception, std.format, std.algorithm.mutation, std.socket;
import core.stdc.stdlib;
import core.thread, core.atomic;
import http.Parser;

import hunt.collection.ByteBuffer;
import hunt.logging;
import hunt.io;
import hunt.util.DateTime;


struct HttpHeader {
	string name, value;
}

struct HttpRequest {
	HttpHeader[] headers;
	HttpMethod method;
	string uri;
}

version(NO_HTTPPARSER) {
enum string ResponseData = "HTTP/1.1 200 OK\r\nContent-Length: 13\r\nConnection: Keep-Alive\r\nContent-Type: text/plain\r\nServer: Hunt/1.0\r\nDate: Wed, 17 Apr 2013 12:00:00 GMT\r\n\r\nHello, World!";
}

abstract class HttpProcessor {
private:
	enum State {
		url,
		field,
		value,
		done
	}

	Appender!(char[]) outBuf;
	HttpHeader[] headers; // buffer for headers
	size_t header; // current header
	string url; // url
	alias Parser = HttpParser!HttpProcessor;
	Parser parser;
	ScratchPad pad;
	HttpRequest request;
	State state;
	bool serving;
	
public:
	TcpStream client;

	this(TcpStream sock) {
		serving = true;
		client = sock;
		headers = new HttpHeader[1];
		pad = ScratchPad(16 * 1024);
		parser = httpParser(this, HttpParserType.request);
	}

	void run() {
		client.onDataReceived((ByteBuffer buffer) { 
			version(NO_HTTPPARSER) {
				client.write(cast(ubyte[])ResponseData);
			} else {
				parser.execute(cast(ubyte[]) buffer.getRemaining());
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

	void onStart(HttpRequest req) {
	}

	void onChunk(HttpRequest req, const(ubyte)[] chunk) {
	}

	void onComplete(HttpRequest req);

	final int onMessageBegin(Parser* parser) {
		outBuf.clear();
		header = 0;
		pad.reset();
		state = State.url;
		return 0;
	}

	final int onUrl(Parser* parser, const(ubyte)[] chunk) {
		pad.put(chunk);
		return 0;
	}

	final int onBody(Parser* parser, const(ubyte)[] chunk) {
		onChunk(request, chunk);
		return 0;
	}

	final int onHeaderField(Parser* parser, const(ubyte)[] chunk) {
		final switch (state) {
		case State.url:
			url = pad.sliceStr;
			break;
		case State.value:
			headers[header].value = pad.sliceStr;
			header += 1;
			if (headers.length <= header)
				headers.length += 1;
			break;
		case State.field:
		case State.done:
			break;
		}
		state = State.field;
		pad.put(chunk);
		return 0;
	}

	final int onHeaderValue(Parser* parser, const(ubyte)[] chunk) {
		if (state == State.field) {
			headers[header].name = pad.sliceStr;
		}
		pad.put(chunk);
		state = State.value;
		return 0;
	}

	final int onHeadersComplete(Parser* parser) {
		headers[header].value = pad.sliceStr;
		header += 1;
		request = HttpRequest(headers[0 .. header], parser.method, url);
		onStart(request);
		state = State.done;
		return 0;
	}

	final int onMessageComplete(Parser* parser) {
		import std.stdio;

		if (state == State.done) {
			try {
				onComplete(request);
			} catch(Exception ex) {
				respondWith(ex.msg, 500);
			}
		}
		if (!parser.shouldKeepAlive)
			serving = false;
		return 0;
	}

}

// ==================================== IMPLEMENTATION DETAILS ==============================================
private:

struct ScratchPad {
	ubyte* ptr;
	size_t capacity;
	size_t last, current;

	this(size_t size) {
		ptr = cast(ubyte*) malloc(size);
		capacity = size;
	}

	void put(const(ubyte)[] slice) {
		enforce(current + slice.length <= capacity, "HTTP headers too long");
		ptr[current .. current + slice.length] = slice[];
		current += slice.length;
	}

	const(ubyte)[] slice() {
		auto data = ptr[last .. current];
		last = current;
		return data;
	}

	string sliceStr() {
		return cast(string) slice;
	}

	void reset() {
		current = 0;
		last = 0;
	}

	@disable this(this);

	~this() {
		free(ptr);
		ptr = null;
	}
}
