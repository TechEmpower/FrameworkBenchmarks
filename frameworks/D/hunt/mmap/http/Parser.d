/// Minimalistic low-overhead wrapper for nodejs/http-parser
/// Used for benchmarks with simple server
module http.Parser;

import http.Common;

import hunt.logging.ConsoleLogger;
import std.conv;
import std.range.primitives;
import core.stdc.string;



/* contains name and value of a header (name == NULL if is a continuing line
 * of a multiline header */
struct phr_header {
    const char *name;
    size_t name_len;
    const char *value;
    size_t value_len;
}

/* returns number of bytes consumed if successful, -2 if request is partial,
 * -1 if failed */
extern (C) pure @nogc nothrow int phr_parse_request(const char *buf, size_t len, const char **method, 
	size_t *method_len, const char **path, size_t *path_len,
    int *minor_version, phr_header *headers, size_t *num_headers, size_t last_len);

/* ditto */
extern (C) pure @nogc nothrow int phr_parse_response(const char *_buf, size_t len, int *minor_version, 
	int *status, const char **msg, size_t *msg_len,
    phr_header *headers, size_t *num_headers, size_t last_len);

/* ditto */
extern (C) pure @nogc nothrow int phr_parse_headers(const char *buf, size_t len, 
	phr_header *headers, size_t *num_headers, size_t last_len);

/* should be zero-filled before start */
struct phr_chunked_decoder {
    size_t bytes_left_in_chunk; /* number of bytes left in current chunk */
    char consume_trailer;       /* if trailing headers should be consumed */
    char _hex_count;
    char _state;
}

/* the function rewrites the buffer given as (buf, bufsz) removing the chunked-
 * encoding headers.  When the function returns without an error, bufsz is
 * updated to the length of the decoded data available.  Applications should
 * repeatedly call the function while it returns -2 (incomplete) every time
 * supplying newly arrived data.  If the end of the chunked-encoded data is
 * found, the function returns a non-negative number indicating the number of
 * octets left undecoded at the tail of the supplied buffer.  Returns -1 on
 * error.
 */
extern (C) pure @nogc nothrow ptrdiff_t phr_decode_chunked(phr_chunked_decoder *decoder, char *buf, size_t *bufsz);

/* returns if the chunked decoder is in middle of chunked data */
extern (C) pure @nogc nothrow int phr_decode_chunked_is_in_data(phr_chunked_decoder *decoder);


// =========== Public interface starts here =============

public:

class HttpException : Exception {
	HttpError error;

	pure @nogc nothrow this(HttpError error, string file = __FILE__,
			size_t line = __LINE__, Throwable nextInChain = null) {
		this.error = error;
		super("Http exception", file, line, nextInChain);
	}
}

struct HttpParser(Interceptor) {
	
private {
	Interceptor interceptor;
	Throwable failure;
	phr_header[50] _headers;
	char *_method;
	char *path;

	int minor_version;
	size_t buflen = 0, prevbuflen = 0, method_len, path_len, num_headers;
}


	alias interceptor this;

	this(Interceptor interceptor) {
		this.interceptor = interceptor;
	}

	@property bool status() pure @safe nothrow {
		return failure is null;
	}

	string uri(bool canCopy=false)() {
		static if(canCopy) {
			return cast(string)path[0..path_len].dup;
		} else {
			return cast(string)path[0..path_len];
		}
	}

	@property HttpMethod method() {
		string s = cast(string)_method[0..method_len];
		return to!HttpMethod(s);
	}


	HttpHeader[] headers(bool canCopy=false)() {
		HttpHeader[] hs = new HttpHeader[num_headers];
		
		for(int i; i<num_headers; i++) {
			phr_header* h = &_headers[i];
			static if(canCopy) {
				hs[i].name = cast(string)h.name[0..h.name_len].idup;
				hs[i].value = cast(string)h.value[0..h.value_len].idup;
			} else {
				hs[i].name = cast(string)h.name[0..h.name_len];
				hs[i].value = cast(string)h.value[0..h.value_len];
			}
		}

		return hs;
	}

	@property bool shouldKeepAlive() pure nothrow {
		return true;
	}

	@property ushort httpMajor() @safe pure nothrow {
		return 1;
	}

	@property ushort httpMinor() @safe pure nothrow {
		return cast(ushort)minor_version;
	}

	int execute(const(ubyte)[] str) {
		return doexecute( str);
	}

	private int doexecute(const(ubyte)[] chunk) {
		failure = null;
		num_headers = cast(int)_headers.length;
		int pret = phr_parse_request(cast(const char*)chunk.ptr, cast(int)chunk.length, 
					&_method, &method_len, 
					&path, &path_len,
					&minor_version, 
					_headers.ptr, &num_headers,
					0);
		debug {
			infof("buffer: %d bytes, request: %d bytes", chunk.length, pret);
		} 

		if(pret > 0) {
			/* successfully parsed the request */
			onMessageComplete();

			if(pret < chunk.length) {
				debug infof("try to parse next request");
				pret += doexecute(chunk[pret .. $]); // try to parse next http request data
			}

			debug infof("pret=%d", pret);
			return pret;
		} else if(pret == -2) {
			debug warning("parsing incomplete");
			num_headers = 0;
			// failure = new HttpException(HttpError.UNKNOWN);
			// throw failure;

			debug infof("pret=%d, chunk=%d", pret, chunk.length);
			return 0;			
		}

		warning("wrong data format");
		num_headers = 0;
		failure = new HttpException(HttpError.UNKNOWN);
		throw failure;
	}

	void onMessageComplete() {
		// interceptor.onHeadersComplete();
		debug {
			tracef("method is %s", _method[0..method_len]);
			tracef("path is %s", path[0..path_len]);
			tracef("HTTP version is 1.%d", minor_version);
			foreach(ref phr_header h; _headers[0..num_headers]) {
				tracef("Header: %s = %s", h.name[0..h.name_len], h.value[0..h.value_len]);
			}
		}
		interceptor.onMessageComplete();
	}
}

auto httpParser(Interceptor)(Interceptor interceptor) {
	return HttpParser!Interceptor(interceptor);
}