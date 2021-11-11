module http.Common;


public enum HttpParserType : uint {
	request = 0,
	response = 1,
	both = 2
}

struct HttpHeader {
	string name, value;
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
	UNKNOWN
}
