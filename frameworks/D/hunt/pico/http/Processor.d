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
import std.array;
import std.string;
import core.stdc.string;
import core.stdc.stdlib;
import std.stdio;
import hunt.io.IoError;
import std.experimental.allocator;
private	alias Parser = HttpParser!HttpProcessor;

void * keepAliveValue;
void * nokeepAliveValue;
long index1;
long index2;
long length1;
long length2;


void * keepAliveJson;
void * nokeepAliveJson;
long index3;
long index4;
long length3;
long length4;


enum RET
{
    TEXT,
    JSON,
    DEF
}


static this()
{
  index1 =  keepAliveResponseData.indexOf("Date:") + 6;
  index2 = nokeepAliveResponseData.indexOf("Date:") + 6;
  length1 = keepAliveResponseData.length;
  length2 = nokeepAliveResponseData.length;
  keepAliveValue = malloc(length1);
  nokeepAliveValue  = malloc(length2);
  memcpy(keepAliveValue , (cast(ubyte[])keepAliveResponseData).ptr, length1);
  memcpy(nokeepAliveValue , (cast(ubyte[])nokeepAliveResponseData).ptr, length2);


  index3 =  keepAliveJsonDate.indexOf("Date:") + 6;
  index4 = nokeepAliveJsonDate.indexOf("Date:") + 6;
  length3 = keepAliveJsonDate.length;
  length4 = nokeepAliveJsonDate.length;
  keepAliveJson = malloc(length3);
  nokeepAliveJson  = malloc(length4);
  memcpy(keepAliveJson , (cast(ubyte[])keepAliveJsonDate).ptr, length3);
  memcpy(nokeepAliveJson , (cast(ubyte[])nokeepAliveJsonDate).ptr, length4);
}

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

//version(NO_HTTPPARSER) {
enum string ResponseData = "HTTP/1.1 200 OK\r\nContent-Length: 13\r\nConnection: Keep-Alive\r\nContent-Type: text/plain\r\nServer: Hunt/1.0\r\nDate: Wed, 17 Apr 2013 12:00:00 GMT\r\n\r\nHello, World!";
//}

enum string keepAliveResponseData = "HTTP/1.1 200 OK\r\nContent-Length: 13\r\nConnection: Keep-Alive\r\nContent-Type: text/plain\r\nServer: Hunt/1.0\r\nDate: Wed, 17 Apr 2013 12:00:00 GMT\r\n\r\nHello, World!";
enum string nokeepAliveResponseData = "HTTP/1.1 200 OK\r\nContent-Length: 13\r\nConnection: close\r\nContent-Type: text/plain\r\nServer: Hunt/1.0\r\nDate: Wed, 17 Apr 2013 12:00:00 GMT\r\n\r\nHello, World!";

enum string keepAliveJsonDate = "HTTP/1.1 200 OK\r\nContent-Length: 27\r\nConnection: Keep-Alive\r\nContent-Type: application/json; charset=UTF-8\r\nServer: Hunt/1.0\r\nDate: Wed, 17 Apr 2013 12:00:00 GMT\r\n\r\n{\"message\":\"Hello, World!\"}";
enum string nokeepAliveJsonDate = "HTTP/1.1 200 OK\r\nContent-Length: 27\r\nConnection: close\r\nContent-Type: application/json; charset=UTF-8\r\nServer: Hunt/1.0\r\nDate: Wed, 17 Apr 2013 12:00:00 GMT\r\n\r\n{\"message\":\"Hello, World!\"}";

abstract class HttpProcessor {

package:
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
    //headers = theAllocator.makeArray!(HttpHeader)(1);
		parser = httpParser(this);
		request.parser = &parser;
    //index1 = keepAliveResponseData.indexOf("Date:") + 6;
    //length1 = keepAliveResponseData.length;
    //length2 = nokeepAliveResponseData.length;
    //keepAliveValue = malloc(length1);
    //nokeepAliveValue = malloc(length2);
    //memcpy(keepAliveValue , (cast(ubyte[])keepAliveResponseData).ptr, length1);
    //memcpy(nokeepAliveValue , (cast(ubyte[])nokeepAliveResponseData).ptr, length2);
	}

	void run() {
		client.received((ByteBuffer buffer) {
			version(NO_HTTPPARSER) {
				client.write(cast(ubyte[])ResponseData);
			} else {
				try {
					int len =  parser.execute(cast(ubyte[]) buffer.getRemaining());
					buffer.position(buffer.position() + len);
				} catch(Exception ex) {
					buffer.clear(); // drop all the  wrong data
					//respondWith(ex.msg, 500);
				}
			}
		})
		.closed(() {
			// notifyClientClosed();
		})
		.error((IoError msg) {
			 warning("Error: ", msg.errorMsg());
		})
		.start();
	}

	protected void notifyClientClosed() {
		debug tracef("The connection[%s] is closed", client.remoteAddress());
	}

	void respondWith(RET type, uint status, HttpHeader[] headers...) {
    switch(type)
    {
      case RET.TEXT:
      {
        if (parser.shouldKeepAlive)
        {
          memcpy(keepAliveValue + index1 , (cast(ubyte[])(DateTimeHelper.getDateAsGMT())).ptr, 29);
          //memcpy(keepAliveValue + index1 , (cast(ubyte[])("Wed, 17 Apr 2013 12:00:00 GMT")).ptr, 29);
          client.write(cast(ubyte[]) keepAliveValue[0 .. length1]);
        }else
        {
          memcpy(nokeepAliveValue + index2 , (cast(ubyte[])(DateTimeHelper.getDateAsGMT())).ptr, 29);
          //memcpy(nokeepAliveValue + index2 , (cast(ubyte[])("Wed, 17 Apr 2013 12:00:00 GMT")).ptr, 29);
          client.write(cast(ubyte[]) nokeepAliveValue[0 .. length2]);
        }
        break;
      }
      case RET.JSON:
      {
        if (parser.shouldKeepAlive)
        {
          memcpy(keepAliveJson + index3 , (cast(ubyte[])(DateTimeHelper.getDateAsGMT())).ptr, 29);
          //memcpy(keepAliveValue + index1 , (cast(ubyte[])("Wed, 17 Apr 2013 12:00:00 GMT")).ptr, 29);
          client.write(cast(ubyte[]) keepAliveJson[0 .. length3]);
        }else
        {
          memcpy(nokeepAliveJson + index4 , (cast(ubyte[])(DateTimeHelper.getDateAsGMT())).ptr, 29);
          //memcpy(nokeepAliveValue + index2 , (cast(ubyte[])("Wed, 17 Apr 2013 12:00:00 GMT")).ptr, 29);
          client.write(cast(ubyte[]) nokeepAliveJson[0 .. length4]);
        }
        break;
      }
      default:
      {

      }
    }

		//return respondWith(cast(const(ubyte)[]) _body, status, headers);
	}

	//void respondWith(const(ubyte)[] _body, uint status, HttpHeader[] headers...) {
	//	outBuf.clear();
	//	formattedWrite(outBuf, "HTTP/1.1 %s OK\r\n", status);
	//	outBuf.put("Server: Hunt/1.0\r\n");
  //
	//	formattedWrite(outBuf, "Date: %s\r\n", DateTimeHelper.getDateAsGMT());
	//	if (!parser.shouldKeepAlive)
	//		outBuf.put("Connection: close\r\n");
	//	foreach (ref hdr; headers) {
	//		outBuf.put(hdr.name);
	//		outBuf.put(": ");
	//		outBuf.put(hdr.value);
	//		outBuf.put("\r\n");
	//	}
	//	formattedWrite(outBuf, "Content-Length: %d\r\n\r\n", _body.length);
	//	outBuf.put(cast(string) _body);
	//	client.write(cast(ubyte[]) outBuf.data); // TODO: short-writes are quite possible
	//}

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
			//respondWith(ex.msg, 500);
		}
		if (!parser.shouldKeepAlive)
			serving = false;
		return 0;
	}

}
