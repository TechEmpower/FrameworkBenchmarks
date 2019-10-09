/// Minimalistic low-overhead wrapper for nodejs/http-parser
/// Used for benchmarks with simple server
module http.Parser;



private:

import std.range.primitives;
import core.stdc.string;

alias http_data_cb = extern (C) int function(http_parser*, const ubyte* at, size_t length);
alias http_cb = extern (C) int function(http_parser*);

public enum HttpParserType : uint {
	request = 0,
	response = 1,
	both = 2
}

public enum HttpMethod : uint {
	DELETE = 0,
	GET = 1,
	HEAD = 2,
	POST = 3,
	PUT = 4,
	/* pathological */
	CONNECT = 5,
	OPTIONS = 6,
	TRACE = 7,
	/* WebDAV */
	COPY = 8,
	LOCK = 9,
	MKCOL = 10,
	MOVE = 11,
	PROPFIND = 12,
	PROPPATCH = 13,
	SEARCH = 14,
	UNLOCK = 15,
	BIND = 16,
	REBIND = 17,
	UNBIND = 18,
	ACL = 19,
	/* subversion */
	REPORT = 20,
	MKACTIVITY = 21,
	CHECKOUT = 22,
	MERGE = 23,
	/* upnp */
	MSEARCH = 24,
	NOTIFY = 25,
	SUBSCRIBE = 26,
	UNSUBSCRIBE = 27,
	/* RFC-5789 */
	PATCH = 28,
	PURGE = 29,
	/* CalDAV */
	MKCALENDAR = 30,
	/* RFC-2068, section 19.6.1.2 */
	LINK = 31,
	UNLINK = 32,
	/* icecast */
	SOURCE = 33,
}

enum HttpError : uint {
	OK,
	/* Callback-related errors */
	CB_message_begin,
	CB_url,
	CB_header_field,
	CB_header_value,
	CB_headers_complete,
	CB_body,
	CB_message_complete,
	CB_status,
	CB_chunk_header,
	CB_chunk_complete,
	/* Parsing-related errors */
	INVALID_EOF_STATE,
	HEADER_OVERFLOW,
	CLOSED_CONNECTION,
	INVALID_VERSION,
	INVALID_STATUS,
	INVALID_METHOD,
	INVALID_URL,
	INVALID_HOST,
	INVALID_PORT,
	INVALID_PATH,
	INVALID_QUERY_STRING,
	INVALID_FRAGMENT,
	LF_EXPECTED,
	INVALID_HEADER_TOKEN,
	INVALID_CONTENT_LENGTH,
	UNEXPECTED_CONTENT_LENGTH,
	INVALID_CHUNK_SIZE,
	INVALID_CONSTANT,
	INVALID_INTERNAL_STATE,
	STRICT,
	PAUSED,
	UNKNOWN,
}

struct http_parser {
	/** PRIVATE **/
	uint state; // bitfield
	uint nread; /* # bytes read in various scenarios */
	ulong content_length; /* # bytes in body (0 if no Content-Length header) */

	/** READ-ONLY **/
	ushort http_major;
	ushort http_minor;
	// bitfield
	uint status_code_method_http_errono_upgrade;
	/** PUBLIC **/
	void* data; /* A pointer to get hook to the "connection" or "socket" object */
}

struct http_parser_settings {
	http_cb on_message_begin;
	http_data_cb on_url;
	http_data_cb on_status;
	http_data_cb on_header_field;
	http_data_cb on_header_value;
	http_cb on_headers_complete;
	http_data_cb on_body;
	http_cb on_message_complete;
	/* When on_chunk_header is called, the current chunk length is stored
   * in parser->content_length.
   */
	http_cb on_chunk_header;
	http_cb on_chunk_complete;
}

extern (C) pure @nogc nothrow void http_parser_init(http_parser* parser, HttpParserType type);

extern (C) pure @nogc nothrow int http_should_keep_alive(const http_parser* parser);

/* Return a string description of the given error */
extern (C) pure @nogc nothrow immutable(char)* http_errno_description(HttpError err);

/* Checks if this is the final chunk of the body. */
extern (C) pure @nogc nothrow int http_body_is_final(const http_parser* parser);

/* Executes the parser. Returns number of parsed bytes. Sets
* `parser->http_errno` on error. */
extern (C) pure @nogc nothrow size_t http_parser_execute(http_parser* parser,
		const http_parser_settings* settings, const ubyte* data, size_t len);

// extern (C) uint http_parser_flags(const http_parser* parser);

uint http_parser_flags(const http_parser* parser) {
	// return parser.status_code | (parser.method<<16) | (parser.http_errno << 24) | (parser.upgrade << 31);
	return parser.status_code_method_http_errono_upgrade;
}

// =========== Public interface starts here =============

public:

class HttpException : Exception {
	HttpError error;

	pure @nogc nothrow this(HttpError error, string file = __FILE__,
			size_t line = __LINE__, Throwable nextInChain = null) {
		this.error = error;
		immutable char* str = http_errno_description(error);
		super(str[0 .. strlen(str)], file, line, nextInChain);
	}
}

struct HttpParser(Interceptor) {
	http_parser parser;
	http_parser_settings settings;
	Interceptor interceptor;
	Throwable failure;
	uint flags;

	static generateCallback(string cName, string dName) {
		import std.format;

		return format(`
			static if(__traits(hasMember, interceptor, "%2$s"))
			{
				extern(C) static int %1$s(http_parser* p) {
					auto parser = cast(HttpParser*)p;
					try {
						parser.flags = http_parser_flags(p);
						return parser.interceptor.%2$s(parser);
					}
					catch (Throwable t) {
						parser.failure = t;
						return 1;
					}
				}
				settings.%1$s = &%1$s;
			}
		`, cName, dName);
	}

	static generateCallbackWithData(string cName, string dName) {
		import std.format;

		return format(`
			static if(__traits(hasMember, interceptor, "%2$s"))
			{
				extern(C) static int %1$s(http_parser* p, const ubyte* at, size_t size) {
					auto parser = cast(HttpParser*)p;
					try {
						parser.flags = http_parser_flags(p);
						return parser.interceptor.%2$s(parser, at[0..size]);
					}
					catch (Throwable t) {
						parser.failure = t;
						return 1;
					}
				}
				settings.%1$s = &%1$s;
			}
		`, cName, dName);
	}

	@property HttpError errorCode() pure @safe nothrow {
		return cast(HttpError)((flags >> 24) & 0x7f);
	}

public:
	alias interceptor this;

	@property uint status() pure @safe nothrow {
		return flags & 0xffff;
	}

	@property HttpMethod method() pure @safe nothrow {
		return cast(HttpMethod)((flags >> 16) & 0xFF);
	}

	this(Interceptor interceptor, HttpParserType type) {
		this.interceptor = interceptor;
		http_parser_init(&parser, type);
		mixin(generateCallback("on_message_begin", "onMessageBegin"));
		mixin(generateCallbackWithData("on_url", "onUrl"));
		mixin(generateCallbackWithData("on_status", "onStatus"));
		mixin(generateCallbackWithData("on_body", "onBody"));
		mixin(generateCallbackWithData("on_header_field", "onHeaderField"));
		mixin(generateCallbackWithData("on_header_value", "onHeaderValue"));
		mixin(generateCallback("on_headers_complete", "onHeadersComplete"));
		mixin(generateCallback("on_message_complete", "onMessageComplete"));
	}

	@property bool shouldKeepAlive() pure nothrow {
		return http_should_keep_alive(&parser) == 1;
	}

	@property ushort httpMajor() @safe pure nothrow {
		return parser.http_major;
	}

	@property ushort httpMinor() @safe pure nothrow {
		return parser.http_minor;
	}

	size_t execute(const(ubyte)[] chunk) {
		size_t size = http_parser_execute(&parser, &settings, chunk.ptr, chunk.length);
		flags = http_parser_flags(&parser);
		if (errorCode) {
			auto f = failure;
			failure = null;
			if (f is null)
				f = new HttpException(errorCode);
			throw f;
		}
		return size;
	}

	size_t execute(const(char)[] str) {
		return execute(cast(const(ubyte)[]) str);
	}
}

auto httpParser(Interceptor)(Interceptor interceptor, HttpParserType type) {
	return HttpParser!Interceptor(interceptor, type);
}
