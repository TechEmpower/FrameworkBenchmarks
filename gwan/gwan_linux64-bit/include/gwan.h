// ============================================================================
// C servlet header for the G-WAN Web Application Server (http://trustleap.ch/)
// ----------------------------------------------------------------------------
// gwan.h: functions exported by G-WAN and made available to C servlets
//         (this file should be located in the gwan/include folder, along
//         with the other include files listed below)
// ============================================================================
#ifndef _GWAN_H
#define _GWAN_H

#ifdef __cplusplus
 extern "C" {
#endif

#include <stdint.h>       // useful types
#include <sys/types.h>    // off_t, needed by http_t struct
#if (__GNUC__ || __INTEL_COMPILER)
# include <float.h>       // limits, conversions
# include <stdarg.h>      // va_start()/va_end()
# include <stddef.h>      // basic types
# include <stdbool.h>     // true/false
# include <time.h>        // time_t is used by G-WAN API calls
#else
# include "float.h"       // limits, conversions
# include "stdarg.h"      // va_start()/va_end()
# include "stddef.h"      // basic types
# include "stdbool.h"     // true/false
#endif
#include "short_types.h"  // u8/s8, u16/s16, u32/int, u64/s64
#include "xbuffer.h"      // xbuf_xcat(), etc.

#ifndef gcc_unused
  #define gcc_unused   __attribute__((__unused__)) // avoid 'unused' warning
#endif

// ----------------------------------------------------------------------------
// handler entry points (declared here for C++, see extern "C" {} above)
// ----------------------------------------------------------------------------
extern int  init (int argc, char *argv[]);
extern int  main (int argc, char *argv[]);
extern void clean(int argc, char *argv[]);

// ============================================================================
// Sections covered below:
// ----------------------------------------------------------------------------
// The 'reply' xbuffer
// The error.log file
// Environment variables
// HTTP response Headers
// HTTP code messages
// URL parameters
// Key-value store
// Memory pools
// Garbage collector
// Handlers
// Cache
// Comet
// Server report
// Run Command
// Comet Streaming
// JSON (de-)serialization
// HTML escaping
// Formatting
// In-memory GIF I/O
// Frame buffer
// Charts & sparklines
// Email
// Time
// Firewall
// Random numbers
// Checksums
// Hashing
// Encryption
// Compression
// ----------------------------------------------------------------------------

// ============================================================================
// The 'reply' xbuffer
// ----------------------------------------------------------------------------
// get the memory pointer for the server reply dynamic buffer
// xbuf_t *reply = get_reply(argv);

xbuf_t *get_reply(char *argv[]);

// use a user-defined allocated (not static) buffer for the server reply
// (usually used to serve entries cached in a G-WAN KV store)
// set_reply(argv, my_reply, my_reply_length, 200); // 200:OK (HTTP status)

void set_reply(char *argv[], char *buf, u32 len, u32 status);

// tell G-WAN when to run a script again (for the same request)
// 'type': WK_MS | WK_FD
#define WK_MS 1  // milliseconds
#define WK_FD 2  // file descriptor
// 'fn' is an optional callback which can be used as a replacement for the
// RC_STREAMING mode which makes a servlet be called repeatedly.
// See the stream1/2/3.c examples
void wake_up(char *argv[], int delay_or_fd, int type, void *fn);

// Setup/update the send() throttling policy with a KiB/second transfer rate.
// This can be done both by servlets and handlers.
//
// 'kbps1' is for the first packet of a reply, 'kbps2' is the speed to use 
// after the first KiBs of the defined 'kbps1' have been sent.
//
// Throttling can be defined on a per-connection basis ('global' == FALSE)
// or globally ('global' == TRUE).
// If you are enabling the 'global' flag then the init() function of a
// listener's connection handler is a good place to modify the default
// Throttling policy (none) one time for all (but this global setting
// can also be modified dynamically by handlers and servlets).
//
// Use 0 to disable throttling (if throttling was previously enabled).
//
// Examples:
// throttle_reply(argv, 100, 150, TRUE); // smaller for 1st packet, faster then
// throttle_reply(argv,   0,   0, TRUE); // disable throttling
// ----------------------------------------------------------------------------
// because of the delay between the timeout loop and the actual processing of
// the event triggered by a write() call, the actual rate is only half of what
// is requested - that' s why we double the values below

void throttle_reply(char *argv[], u16 kbps1, u16 kbps2, int global);

// return the connection context of the given file descriptor 'fd2' peered
// to 'fd1' (the client connection created earlier); if the fd 'fd1' is 
// unknown or irrelevant (no relaying taking place) then use 0 for 'fd1'.
// (used by "Protocol Handlers", see the /handlers/RELAY.c example)

void *get_fd_ctx(int fd2, int fd1);

// ============================================================================
// The error.log file
// ----------------------------------------------------------------------------
// output text in the current virtual host 'error.log' file:
// char str[256];
// u64 nbr_records = 1234567890123;
// s_snprintf(str, sizeof(str)-1, "database records: %llu", nbr_records);
// log_err(argv, str);

void log_err(char *argv[], const char *msg);

// ============================================================================
// Environment variables
// ----------------------------------------------------------------------------
// get an environment variable (a performance counter, or a persistence ptr)
// (see the 'contact.c' example and 'enum HTTP_Env' below for all values)
//
// 'inval' is only used to get a *pointer* on a value (so we can change the
// value, like for HTTP_CODE, DOWNLOAD_SPEED, US_HANDLER_DATA, US_VHOST_DATA,
// or US_HANDLER_STATES):

// int session = get_env(argv, SESSION_ID); // easy cases
// char *www   = get_env(argv, WWW_ROOT);

// int **pHttp_code = get_env(argv, HTTP_CODE);
// if(pHttp_code)       // if we got a pointer on the value,
//   *pHttp_code = 200; // then change the value (to 200:OK)

// int **pDownload_speed = 0;
// get_env(argv, DOWNLOAD_SPEED);
// if(pDownload_speed)     // if we got a pointer on the value,
//   *pDownload_speed = 2; // then change the value (to 2,048 bytes per sec)

// void **pVhost_persistent_ptr = get_env(argv, US_VHOST_DATA);
// if(pVhost_persistent_ptr) // if we got a pointer on the value, change it
//   *pVhost_persistent_ptr = strdup("persistent data");
//
u64 get_env(char *argv[], int name);

// these codes supplement the standard HTTP codes, above and beyond the 
// reserved ]100-600[ range and let servlets alter the normal behavior
//
enum HTTP_codes
{
  RC_CLOSE     = 0,    // close connection

  RC_NOHEADERS = 1,    // prevent G-WAN from injecting HTTP headers
                       // [100-600] return codes are for HTTP status codes

  RC_STREAMING = 1000, // call script again after all reply was sent

  RC_NOCACHE   = 2000, // prevent G-WAN from caching servlet output
};

// see the 'PONG.c' protocol handler example
//
enum PROTOCOL_state
{
   PRT_CONNECTED = 1, // a new (client or backend) connection was established
   PRT_ACCEPTED,      // a new (client or backend) connection was accepted
   PRT_SRV_READ,      // client  data can be read from the input  socket buffer
   PRT_SRV_WRITE,     // client  data can be sent to   the output socket buffer
   PRT_PRX_READ,      // backend data can be read from the input  socket buffer
   PRT_PRX_WRITE,     // backend data can be sent to   the output socket buffer
   PRT_CLOSED,        // the (client or backend) connection was closed
};

enum HTTP_Method
{
    HTTP_ANY=0, HTTP_GET, // RFC 2616
    HTTP_HEAD, HTTP_POST, HTTP_PUT, HTTP_DELETE, HTTP_OPTIONS, HTTP_PATCH,
    // G-WAN currently supports this list until 'PATCH'
    HTTP_CONNECT,
    HTTP_TRACE,
    HTTP_PROPFIND,  // RFC 2518: WebDAV
    HTTP_PROPPATCH,
    HTTP_MKCOL,
    HTTP_COPY,
    HTTP_MOVE,
    HTTP_LOCK,
    HTTP_UNLOCK,
    HTTP_VERSION_CONTROL,
    HTTP_CHECKOUT,
    HTTP_UNCHECKOUT,
    HTTP_CHECKIN,
    HTTP_UPDATE,
    HTTP_LABEL,
    HTTP_REPORT,
    HTTP_MKWORKSPACE,
    HTTP_MKACTIVITY,
    HTTP_BASELINE_CONTROL,
    HTTP_MERGE,
    HTTP_INVALID    // RFC 3253: WebDAV versioning
};

/* letting you translate HTTP method codes into character strings (tracing)
static char *szHTTP_Method[] =
{
   [HTTP_ANY]        = "?",
   [HTTP_GET]        = "GET",
   [HTTP_HEAD]       = "HEAD",
   [HTTP_POST]       = "POST",
   [HTTP_PUT]        = "PUT",
   [HTTP_DELETE]     = "DELETE",
   [HTTP_OPTIONS]    = "OPTIONS",
   [HTTP_PATCH]      = "PATCH",
   // G-WAN currently supports this list until 'OPTIONS'
   [HTTP_CONNECT]    = "CONNECT",
   [HTTP_TRACE]      = "TRACE",
   [HTTP_PROPFIND]   = "PROPFIND",  // RFC 2518: WebDAV
   [HTTP_PROPPATCH]  = "PROPPATCH",
   [HTTP_MKCOL]      = "MKCOL",
   [HTTP_COPY]       = "COPY",
   [HTTP_MOVE]       = "MOVE",
   [HTTP_LOCK]       = "LOCK",
   [HTTP_UNLOCK]     = "UNLOCK",
   [HTTP_VERSION_CONTROL]="VERSION_CONTROL",
   [HTTP_CHECKOUT]   = "CHECKOUT",
   [HTTP_UNCHECKOUT] = "UNCHECKOUT",
   [HTTP_CHECKIN]    = "CHECKIN",
   [HTTP_UPDATE]     = "UPDATE",
   [HTTP_LABEL]      = "LABEL",
   [HTTP_REPORT]     = "REPORT",
   [HTTP_MKWORKSPACE]= "MKWORKSPACE",
   [HTTP_MKACTIVITY] = "MKACTIVITY",
   [HTTP_BASELINE_CONTROL]="BASELINE_CONTROL",
   [HTTP_MERGE]      = "MERGE",
   [HTTP_INVALID]    = "INVALID"    // RFC 3253: WebDAV versioning
   ""
};*/

enum HTTP_Type
{
   TYPE_IDENTITY,   // plain text (default)
   TYPE_URLENCODED, // "&name=toto"
   TYPE_MULTIPART,  // headers + base64
   TYPE_OCTETSTREAM // ASCII/binary
};

enum ENC_Type
{
   ENC_IDENTITY = 0, // plain text (default)
   ENC_GZIP     = 1, // MUST stay at 1, see ((6 << 1) + ENC_GZIP)
   ENC_DEFLATE  = 2,
   ENC_COMPRESS = 4,
   ENC_CHUNKED  = 8,
   ENC_SDCH     = 16
};

enum AUTH_Type
{
   AUTH_BAD    = 0,
   AUTH_ANY    = 1, 
   AUTH_BASIC  = 2, 
   AUTH_DIGEST = 3,
   AUTH_SRP    = 4, // DH-like authentication and key exchange (shared secret)
   AUTH_X509   = 5 // can be used on the top of AUTH_BASIC/AUTH_DIGEST/AUTH_SRP
};

enum
{
   LG_INNER, // a routine natively implemented (in C) in the G-WAN server
   LG_ADA,
   LG_ASM,
   LG_BASIC,
   LG_C,
   LG_COBOL,
   LG_CPP,
   LG_CS,
   LG_D,
   LG_FORTRAN,
   LG_GO,
   LG_JAVA,
   LG_JS,
   LG_LUA,
   LG_MERCURY,
   LG_MODULA,
   LG_OBJC,
   LG_OBJCPP,
   LG_PASCAL,
   LG_PERL,
   LG_PHP,
   LG_PLI,
   LG_PYTHON,
   LG_RUBY,
   LG_SCALA,
   LG_SCHEME,
   LG_VHDL
};

// the HTTP state of a connection
// (see the served_from.c example for how to use it)
typedef struct
{
   off_t    h_range_from,
            h_range_to;
   u32      session_id;    // csp: only used by get_env() at the moment...
   int      file_fd;       // needs 32 bits as it is tested for -1 equality...
   u32      h_if_modified, h_if_unmodified, // EPOCH time (in seconds)
            h_content_length;
   u32      h_entity            :12, // 0-4096 (room for many HTTP headers,
            h_host              :12, // G-WAN rejects requests with too many)
            h_useragent         :12,
            h_referer           :12,
            h_accept_language   :12,
            h_cookies           :12,
            h_if_nonematch      :12, // If-None-Match: "68689-7696a7c-876b7e" (ETag)
            h_auth_type         : 3, // 0-7
            h_keepalive         : 1, // 0-1
            h_accept_encoding   : 4, // 0-15
            h_content_encoding  : 4, // 0-15
            h_transfer_encoding : 4, // 0-15
            h_port              :16, // 0-65,535 (server port)
            h_content_type      : 2, // 0-3
            h_expect            : 1, // 0-1
            h_maj_ver           : 1, // 0-1
            h_min_ver           : 4, // 0-15
            h_ver               :10, // 0-1023
            h_method            : 5, // 0-32 (0-27 to cover the HTTP standard)
            pipelined           : 1, // 0-1  (another request follows)
            h_do_not_track      : 1; // 0-1  (optional W3C "DNT:" header)

   char    *h_auth_user, *h_auth_pwd;

#ifdef _USE_SSL
   ssl_ctx *ssl;
#endif
} http_t;

enum http_env
{
   // -------------------------------------------------------------------------
   // Server 'environment' variables
   // -------------------------------------------------------------------------
   // NOTE: QUERY_CHAR and DEFAULT_LANG are global settings (server-wide)
   REQUEST = 0,     // char  *REQUEST;        // "GET / HTTP/1.1\r\n..."
   REQUEST_LEN,     // int    REQUEST_LEN     // strlen(REQUEST); with headers
   REQUEST_METHOD,  // int    REQUEST_METHOD  // 1=GET, 2=HEAD, 3=PUT, 4=POST
   QUERY_STRING,    // char  *QUERY_STRING    // request URL after first '?'
   FRAGMENT_ID,     // char  *FRAGMENT_ID     // request URL after last '#'
   REQ_ENTITY,      // char  *REQ_ENTITY      // "arg=x&arg=y..."
   CONTENT_TYPE,    // int    CONTENT_TYPE    // 1="x-www-form-urlencoded"
   CONTENT_LENGTH,  // int    CONTENT_LENGTH  // body length provided by client
   CONTENT_ENCODING,// int    CONTENT_ENCODING// entity, gzip, deflate
   SESSION_ID,      // int    SESSION_ID;     // 12345678 (range: 0-4294967295)
   HTTP_CODE,       // int   *HTTP_CODE;      // 100-600 range (200:'OK')
   HTTP_HEADERS,    // struct *http_t;        // see struct http_t above
   AUTH_TYPE,       // int    AUTH_TYPE;      // see enum AUTH_Type {}
   REMOTE_ADDR,     // char  *REMOTE_ADDR;    // "192.168.54.128"
   REMOTE_BIN_ADDR, // u64    REMOTE_BIN_ADDR;// u64 ip = numeric_ip_address;
   REMOTE_PORT,     // int    REMOTE_PORT;    // 1460 (range: 1024-65535)
   REMOTE_PROTOCOL, // int    REMOTE_PROTOCOL // ((HTTP_major*1000)+HTTP_minor)
   REMOTE_USER,     // char  *REMOTE_USER     // "Pierre"
   REMOTE_PWD,      // char  *REMOTE_PWD      // "secret"
   CLIENT_SOCKET,   // int    CLIENT_SOCKET   // 1032 (-1 if invalid/closed)
   USER_AGENT,      // char  *USER_AGENT;     // "Mozilla ... Firefox"
   SERVER_SOFTWARE, // char  *SERVER_SOFTWARE // "G-WAN/1.0.2"
   SERVER_NAME,     // char  *SERVER_NAME;    // "domain.com"
   SERVER_ADDR,     // char  *SERVER_ADDR;    // "192.168.10.14"
   SERVER_PORT,     // int    SERVER_PORT;    // 80 (443, 8080, etc.)
   SERVER_DATE,     // char  *SERVER_DATE;    // "Tue, 06 Jan 2009 06:12:20 GMT"
   SERVER_PROTOCOL, // int    SERVER_PROTOCOL // ((HTTP_major*1000)+HTTP_minor)
   VHOST_ROOT,      // char  *VHOST_ROOT;     // the (virtual) host root folder
   WWW_ROOT,        // char  *WWW_ROOT;       // the HTML pages root folder
   CSP_ROOT,        // char  *CSP_ROOT;       // the CSP .C files folder
   LOG_ROOT,        // char  *LOG_ROOT;       // the log files folder
   HLD_ROOT,        // char  *HLD_ROOT;       // the handlers folder
   FNT_ROOT,        // char  *FNT_ROOT;       // the fonts folder
   DOWNLOAD_SPEED,  // int   *DOWNLOAD_SPEED; // minimum allowed transfer rate
   MIN_READ_RATE = DOWNLOAD_SPEED,            //
   READ_XBUF,       // xbuf_t*READ_XBUF;      // pointer to the read() xbuffer
   SCRIPT_TMO,      // u32   *SCRIPT_TMO;     // time-out in milliseconds
   KALIVE_TMO,      // u32   *KALIVE_TMO;     // time-out in milliseconds
   REQUEST_TMO,     // u32   *REQUEST_TMO;    // time-out in milliseconds
   MIN_READ_SPEED,  // u32   *MIN_READ_SPD;   // rate in bytes per second
   NBR_CPUS,        // int    NBR_CPUS;       // total of available CPUs
   NBR_CORES,       // int    NBR_CORES;      // total of available CPU Cores
   NBR_WORKERS,     // int    NBR_WORKERS;    // total of server workers
   CUR_WORKER,      // int    CUR_WORKER;     // worker thread number: 1,2,3...
   REPLY_MIME_TYPE, // char  *REPLY_MIME_TYPE;// set script's reply MIME type
   DEFAULT_LANG,    // u8     DEFAULT_LANG;   // CC_D: /?hello.d => /?hello
   QUERY_CHAR,      // u8     QUERY_CHAR;     // replace '?' by [ -_.!~*'() ]
   REQUEST_TIME,    // u64    REQUEST_TIME;   // time (parse+build) in microsec
   MAX_ENTITY_SIZE, // u32   *MAX_ENTITY_SIZE;// maximum POST entity size
   USE_WWW_CACHE,   // u8    *USE_WWW_CACHE;  // 0:disabled (default) 1:enabled
   USE_CSP_CACHE,   // u8    *USE_CSP_CACHE;  // 0:disabled (default) 1:enabled
   CACHE_ALL_WWW,   // u8    *CACHE_ALL_WWW;  // 1:cache all /www at startup
   USE_MINIFYING,   // u8    *USE_MINIFYING;  // '1' by default (JS/CSS/HTML)
   // -------------------------------------------------------------------------
   // Server performance counters
   // -------------------------------------------------------------------------
   CC_BYTES_IN = 100, CC_BYTES_OUT,  CC_ACCEPTED,  CC_CLOSED,   CC_REQUESTS,
   CC_HTTP_REQ,       CC_CACHE_MISS, CC_ACPT_TMO,  CC_READ_TMO, CC_SLOW_TMO,
   CC_SEND_TMO,       CC_BUILD_TMO,  CC_CLOSE_TMO, CC_CSP_REQ,  CC_STAT_REQ,
   CC_HTTP_ERR,       CC_EXCEPTIONS, CC_BYTES_INDAY, CC_BYTES_OUTDAY,
   // -------------------------------------------------------------------------
   // Handler and VirtualHost Persistence pointers
   // -------------------------------------------------------------------------
   US_REQUEST_DATA = 200, // Request-wide pointer
   US_HANDLER_DATA,       // Listener-wide pointer
   US_VHOST_DATA,         // VirtualHost-wide pointer
   US_SERVER_DATA,        // Server-wide pointer (global)
   US_HANDLER_STATES,     // states registered to get server-state notifications
   US_HANDLER_CTX_SIZE    // size of "Protocol Handler" per-connection context
};

// ============================================================================
// Protocol Buffers: connection context structure (do not modify it)
// ----------------------------------------------------------------------------
typedef struct { u8  str[12]; u32 num; } ip_t;
typedef struct
{
   ip_t   client_ip;    // "a.b.c.d" + IP address as u32
   int    client_fd;    // client socket we accepted from client
   u32    id       :20, // 0- 1m+, client_fd initial value, even after close()
          state    :12, // 0- 4095, connection state
          reserved :32; // do not modify!
   void  *ctx;          // user-defined protocol-handler context
} conn_t;

// ============================================================================
// HTTP response Headers
// ----------------------------------------------------------------------------
// modify HTTP response headers generated by G-WAN
// example: char header[] = "Powered-by: ANSI C scripts\r\n";
//          http_header(HEAD_ADD, header, sizeof(header) - 1, argv);

enum HEADERS_flags
{
   HEAD_ADD   = 1, // add this HTTP header to response headers
   HEAD_MOD   = 2, // not implemented yet
   HEAD_DEL   = 4, // not implemented yet
   HEAD_AFTER = 8  // add this data chunck just after [HTTP headers CRLFCRLF]
};

// *only* for STATIC requests used by "content-type" handlers
void http_header(u32 flags, char *buf, u32 buflen, char *argv[]);

// build HTTP response headers (usually used with set_reply(), see the
// main_404.cxx handler example)
//
// char *date = get_env(argv, SERVER_DATE, 0);
// char szmodified[32];
// static const char buf[] =
//    "HTTP/1.1 %s\r\n"
//    "Date: %s\r\n"
//    "Last-Modified: %s\r\n"
//    "Content-type: text/html\r\n"
//    "Content-Length: %u\r\n" // HTML body length
//    "Connection: keep-alive\r\n\r\n";
//
// build_headers(argv, buf,
//           http_status(*pHTTP_status), // "200 OK" here
//           date,                       // current HTTP time
//           time2rfc(mod, szmodified),  // file HTTP time
//           len);                       // file length
//
// set_reply(argv, c, len, *pHTTP_status); // re-use cached buffer

void build_headers(char *argv[], char *format, ...);

// ============================================================================
// HTTP code messages
// ----------------------------------------------------------------------------
// example:
// char *hdr, *msg;
// hdr = http_status(404); // "404 Not Found"
// msg = http_error(404); // "The requested URL was not found on this server."
char *http_status(int code);
char *http_error (int code);

// ============================================================================
// URL parameters
// ----------------------------------------------------------------------------
// get an URL parameter: "http://127.0.0.1/csp?hellox&name=Eva"
// example: char *name = 0; get_arg("name=", &name, argc, argv);
//          (now, 'name' points to "Eva" - you MUST check if 'name' is NULL)

void get_arg(char *name, char **value, int argc, char *argv[]);

// you can also walk main()'s argv[] values:
// int i = 0;
// while(i < argc)
// {
//    xbuf_xcat(reply, "argv[%u] '%s'<br>", i, argv[i]);
//    i++;
// }

// ============================================================================
// Server report
// ----------------------------------------------------------------------------
// append a server report (in html or text format) to an xbuffer)
// most of this data can be extracted from the performance counters above but
// this is a convenient way to get a 'snapshot' of the server state
void server_report(xbuf_t *reply, int html); // see the report.c example
void mpools_report(xbuf_t *reply);           // see the mpools.c example

// ============================================================================
// Run Command
// ----------------------------------------------------------------------------
// Let a G-WAN script run an external command like 'ping' and get a file
// descriptor to read() the command's output. GLIBC's popen() does not fit
// the task here because it buffers the command output.
// see the stream3.c example
//
int run_cmd(const char *cmd, int argc, char *argv[]);

// ============================================================================
// Key-Value store
// ----------------------------------------------------------------------------
// example: (for more details, see the kv.c example)
//
// kv_item item;
// item.key = strdup("Paula");
// item.val = strdup("Accounting");
// item.klen = sizeof("Paula");
// item.flags = 0;
//
// kv_t store;
// kv_new(&store, 4 * 1024, argv); // using 4 KB "maximum"
// kv_add(&store, &item, argv);
//
// char *ptr = kv_get(&store, "Paula");
// if(ptr)
//    printf("value: %s\n", ptr);
//
// kv_del(&store, "Paula");
// kv_free(&store); // makes the above kv_del() redundant in this example
// ----------------------------------------------------------------------------
// the kv_init() flag options
// (only KV_REFCOUNT and KV_NO_UPDATE are implemented in the kv code; the rest
//  of these flags are there to let people keep KV callbacks and flags in the
//  kv context rather than in a redundant structure - for those flags to have
//  any effect, G-WAN users must write the corresponding code)
//
enum KV_OPTIONS
{
   KV_REFCOUNT    =   1, // reference count, incremented by kv_get()
   KV_PERSISTANCE =   2, // periodic file I/O (using kv_recfn() callback)
   KV_INCR_KEY    =   4, // 1st field:primary key (automatically incremented)
   KV_CUR_TIME    =   8, // 2nd field:time stamp  (automatically generated)
   KV_NO_UPDATE   =  16, // make kv_add() fail to update an existing entry
   KV_SHARED      =  32, // available outside of G-WAN
   KV_DISTRIBUTED =  64, // relying on a distributed topology
   
   KV_PREFIX      = 128, // kv_get() will return best entry (not implemented)
   KV_SIMILAR     = 256, // kv_get() will return best entry (not implemented)
   KV_NEXT        = 512, // kv_get() will return best entry (not implemented)
   KV_PREV        =1024  // kv_get() will return best entry (not implemented)
};

// a key-value store

typedef struct
{
   char  name[12];   // kv name (optional)
   u32   flags;      // kv flags (optional)
   long  root;       // kv storage
   long  nbr_items;  // nbr of items in kv (atomically maintained)
   long  lock;       // global kv lock (not used by G-WAN, only for users)
   void *delfn;      // kv_del() callback (to free memory of custom records)
   void *recfn;      // persistence callback (to format records saved to disk)
   void *pool;       // if specified, used for memory allocations
} kv_t;

// a tuple (key-value, and key-value lengths)
// if(!klen) then kv_add()/kv_get()/kv_del()/kv_do() do klen = strlen(klen);
//
typedef struct
{
   char *key, *val;

   union{
   u32  flags;  // kv_add() flags: set to zero for compatibility
   u32  in_use; // if(KV_REFCOUNT) ref_count atomically incr. by kv_get()
   };           // (when update/delete, if(!in_use) free(val); )

   u32  klen;   // 0:marked as to-be-deleted (when not in_use)
} kv_item;

// delfn is an user-defined function to free memory allocated for KV records
typedef void(*kv_delfn_t)(void *value);

// recfn is an user-defined function to format KV records when saved to disk
typedef void(*kv_recfn_t)(void *value);

// create a KV store (all arguments can be NULL but 'store')
void kv_init(kv_t *store, char *name, long max_nbr_items, u32 flags,
             kv_delfn_t delfn, kv_recfn_t recfn);

// add/update a value associated to a key
// return: 0:out of memory, else:pointer on existing/inserted kv_item struct
kv_item *kv_add(kv_t *store, kv_item *item);

// search a 'value' using a 'key'
// return: 0:not found, else:pointer on the value we found
char *kv_get(kv_t *store, const char *key, int klen);

// delete a 'value' using a 'key'
// return: 0:failure (value not found), 1:success
int kv_del(kv_t *store, const char *key, int klen);

// free all the keys and values
void kv_free(kv_t *store);

// run 'proc(kval_node, ctx)' on the specified subset of entries
//
// if 'key' is NULL then we visit all the entries,
// else we only visit the entries that start with the 'key' string
// (entries are visited in ASCII/binary order)
// if(!klen) then kv_do() does klen = strlen(klen);
//
// return:
//  1: success: visited all matching entries
//  2: no entry starts with the 'key' string
//
// It must return 1 to continue searching (any other value stops the search)
// You can use the 'user_defined_ctx' context to store a structure for
// counters, data collection, etc.

typedef int(*kv_proc_t)(const kv_item *item, const void *user_defined_ctx);

int kv_do(kv_t *store, const char *key, int klen, kv_proc_t kv_proc,
          void *user_defined_ctx);

// ============================================================================
// Memory pools
// ----------------------------------------------------------------------------
// This is useful to pre-allocate a heap that can be freed with mp_del();
// without having to free() all the allocated blocks one by one and with
// the desirable side effect to really release the memory to the system
// (LIBC releases the memory to the application heap, not to the system).
//
// Also, if your initial pool size is too small to satisfy mp_malloc();
// then the pool is automatically expanded. This is especially useful
// with the G-WAN KV store:
//
// kv_t kv;
// kv_init(&kv, ...);
// kv.pool = mp_new(...); // useful to free the memory later (must be done!)
//
// Generic use:
//
// void *pool = mp_init(1024 * 1024); // 1 MB memory pool
// char *str = mp_malloc(pool, 255);  // get some bytes
// str[0] = 0;                        // use the bytes
// mp_free(pool, str);                // free the bytes for another mp_malloc()
// mp_del(pool);                      // delete the pool
// ----------------------------------------------------------------------------
void *mp_init  (size_t pool_size);
void  mp_del   (void *pool);
void *mp_malloc(void *pool, size_t size);
void  mp_free  (void *pool, void *ptr);

// ============================================================================
// Garbage collector
// ----------------------------------------------------------------------------
// NOTE: this memory CANNOT BE SHARED between connections as allocated
//       blocks are automatically deleted when a request is completed.
// 
// MUST be used to define the total allocated memory needed by your servlet
// (you must check that gc_init() succeeded to make room for the memory,
//  0:failure, else the amount of allocated bytes is returned)
int   gc_init  (char *argv[], size_t size);

// gc_malloc and gc_free() work as expected: you really free memory
// that can be re-used in a loop for example; as a bonus, gc_free()
// does not crash with double-free or invalid pointer calls.
void *gc_malloc(char *argv[], size_t size);
void  gc_free  (char *argv[], void *ptr);

// ============================================================================
// G-WAN server handlers
// ----------------------------------------------------------------------------
// define which handler states we want to be notified in the handler's main():
enum HANDLER_ACT
{
   HDL_INIT = 1,     // skip 0: +/- values for connection/protocol handlers
   HDL_AFTER_ACCEPT, // just after accept (only client IP address setup)
   HDL_AFTER_READ,   // each time a read was done until HTTP request OK
   HDL_BEFORE_PARSE, // HTTP verb/URI validated but HTTP headers are not
   HDL_AFTER_PARSE,  // HTTP headers validated, ready to build reply
   HDL_BEFORE_WRITE, // after a reply was built, but before it is sent
   HDL_STREAMING,    // when we are sending a reply progressively
   HDL_AFTER_WRITE,  // after a reply was sent
   HDL_HTTP_ERRORS,  // when G-WAN is going to reply with an HTTP error
   HDL_BEFORE_CLOSE, // before closing a connection
   HDL_CLEANUP
};

// ============================================================================
// Cache
// ----------------------------------------------------------------------------
// create/update a cache entry ('file' MUST be imaginary if 'buf' is not NULL)
// cacheadd(argv, "/tool/counter", buf, 1024, ".json", 200, 60); // expire:60sec
// cacheadd(argv, "/archives/doc_1.pdf", 0, 0, 0, 200, 0); // never expire
//                             ('file' MUST exist if 'buf' is NULL)
// if(expire == 0) never expires
// if(expire >  0) expires in 'expires' seconds
//
// 'mime' is the FILE EXTENSION like ".gif" (not the "image/gif" MIME type)
//        This is useful when a resource name does not match the MIME type:
//
//        cacheadd(argv, "/tool/counter", buf, 1024, ".json", 200, 60);
//
// 'code' is the HTTP status code that the server will send to clients
//
// return 0:failure, !=0:success
//
// see the cache1.c, cache2.c, etc. samples.

long cacheadd(char *argv[], char *file, char *buf, u32 buflen, char *mime, 
              u32 code, u32 expire);

// delete a cached entry
// example: cachedel(argv, "/tool/counter");

void cachedel(char *argv[], char *file);

// search a cached entry (and, if found, return the requested details)
// examples:
//    char *mime = 0;
//    u32 len = 0, code = 0, date = 0, exp = 0;
//    char *entry = cacheget(argv, "tool/counter", &len, &mime, &code, &date, &exp);
//    char *entry = cacheget(argv, "tool/counter", &len, &mime, 0, 0, 0);
//    char *entry = cacheget(argv, "tool/counter", 0, 0, 0, 0, 0);
// return 0:not found, else pointer on cached entry
//
// NOTES: a cached entry can disappear at any moment IF IT EXPIRES. In that
//        case, you MUST MAKE A COPY of the returned buffer to use it.
//        DON'T work 'inplace' in the provided buffer, rather make a copy
//        and call cacheadd() to update a previously cached entry.
//
//        The returned 'mime' argument, if any, is the ".gif" file extension
//        and not the "image/gif" MIME type. This is useful when a resource
//        name does not match the MIME type:
//
//        cacheadd(argv, "/tool/counter", buf, 1024, ".json", 200, 60);

char *cacheget(char *argv[], char *uri, u32 *buflen, char **mime, u32 *code,
               u32 *modified, u32 *expire);

// ============================================================================
// Comet Streaming
// ----------------------------------------------------------------------------
// see the comet.c example for how to setup a feed and to add subscribers
//

typedef int (*make_data_t)(char *argv[]);
typedef int (*push_data_t)(char *argv[], xbuf_t *reply);
typedef void(*free_data_t)(char *argv[]);

int push_list_add(char *argv[], char *feed_name,
                  make_data_t make_fn, u32 make_freq,
                  push_data_t push_fn, u32 push_freq,
                  free_data_t free_fn);

// ============================================================================
// JSON (de-)serialization
// ----------------------------------------------------------------------------
// NOTE: numbers are stored as 'double', don't forget to *cast* in xbuf_xcat()
// see json.c for an extensive example (it uses all the functions below)

enum JSN_TYPE
{
   jsn_FALSE = 0, jsn_TRUE, jsn_NULL, jsn_INTEGER, jsn_REAL, jsn_STRING,
   jsn_NODE, jsn_ARRAY
};

typedef struct jsn_s jsn_t;
struct jsn_s
{
   jsn_t *prev;    // node's prev item (parent if node is 1st child)
   jsn_t *next;    // node's next item (list ends with NULL)
   jsn_t *child;   // node's child node (NULL if none)
   char  *name;    // node's name
   int    type;    // node's value type (see JSN_TYPE above)
   union {
   char  *string;  // value 'type' == jsn_STRING
   s64    integer; // value 'type' == jsn_INTEGER


   double real;    // value 'type' == jsn_REAL
   };
   u64    x;       // context
   long   y;       // context
};

// helpers for code clarity
#define jsn_add_null(node, name)       jsn_add(node, name, jsn_NULL,    0)
#define jsn_add_false(node, name)      jsn_add(node, name, jsn_FALSE,   0)
#define jsn_add_true(node, name)       jsn_add(node, name, jsn_TRUE,    0)
#define jsn_add_bool(node, name, n)    jsn_add(node, name, (n != 0),    0)
#define jsn_add_string(node, name, s)  jsn_add(node, name, jsn_STRING, (long)s)
#define jsn_add_node(node, name)       jsn_add(node, name, jsn_NODE,    0)
#define jsn_add_array(node, name, n)   jsn_add(node, name, jsn_ARRAY,   n)
#define jsn_add_integer(node, name, n) jsn_add(node, name, jsn_INTEGER, n)

// take JSON text as input and return a jsn_t tree
// (call jsn_free() when you are done with the jsn_t tree)
jsn_t *jsn_frtext(char *text, char *name);

// append and format a jsn_t object into text in the specified dynamic buffer
// if(!formated) then a compact formating is used (no separator, CRLF)
// (call xbuf_free(text) when you are done with the text)
char *jsn_totext(xbuf_t *text, jsn_t *node, int formated);

// return a node's item[i] or NULL if item[i] does not exist
jsn_t *jsn_byindex(jsn_t *node, int i);

// search for 'name' in all same-level items; if(deep) in children
// (case insensitive search)
jsn_t *jsn_byname(jsn_t *node, char *name, int deep);

// search for 'value' of 'type' in all same-level items; if(deep) in children
// (case insensitive search)
jsn_t *jsn_byvalue(jsn_t *node, int type, double value, int deep);

// add an 'item' or a 'node' to the specified node
jsn_t *jsn_add     (jsn_t *node, char *name, int type, u64 value);

// modern compilers no longer allow double to void* casts...
jsn_t *jsn_add_real(jsn_t *node, char *name, double value);

// update an 'item' or a 'node'
jsn_t *jsn_updt(jsn_t *node, double value);

// remove an 'item' or a 'node' (and all its nodes/items)
void jsn_del(jsn_t *node);

// free all memory and delete a jsn_t node and all its nodes and items
void jsn_free(jsn_t *node);

// ============================================================================
// HTML escaping
// ----------------------------------------------------------------------------
u32  url_encode   (u8 *dst, u8 *src, u32 maxdstlen);  // return len
u32  escape_html  (u8 *dst, u8 *src, u32 maxdstlen);  // return len

u32  unescape_html(u8 *str);                          // inplace, return len
int  html2txt     (u8 *html, u8 *text, int maxtxlen); // return len

// ============================================================================
// Formatting
// ----------------------------------------------------------------------------
// extended sprintf(): (these extensions are also used by xbuf_xcat())
// "%F","%D","%I","%U" - pretty thousands (the ' formatter is also supported)
// "%b"                - binary (8 => "1000")
// "%B","%-B"          - base 64 encode/decode ("%12B" encode a binary buffer)
// "%3C"               - generate a string of length n ("%3C", 'A' => "AAA")
// "%k"                - KB, MB, GB, etc. (1024 => "1 KB")
// "%m"                - strerror_r() system error messages (errno)

int s_snprintf (char *str, size_t len, const char *fmt, ...);
int s_vsnprintf(char *str, size_t len, const char *fmt, va_list a);

// ============================================================================
// In-memory GIF I/O
// ----------------------------------------------------------------------------
// to save a GIF image on disk, just save the buffer made by gif_build()

// build an in-memory GIF image from a raw 'bitmap'
// params: buffer      - destination buffer (must be pre-allocated)
//         bitmap      - input pixels (8-bit pixels)
//         width       - image width
//         height      - image height
//         palette     - color palette (3 * 256 = 768 bytes maximum)
//         nbrcolors   - number of entries in palette (256 maximum)
//         transparent - index of transparent colour (-1: no transparency)
//         comment     - a pointer on a text comment (0:none)
// return: length of GIF image, -1 if failure (see the fractal.c example)

int gif_build(u8 *gif, u8 *bitmap, u32 width, u32 height, u8 *palette,
              u32 nbcolors, int transparency, u8 *comment);

// split an in-memory GIF (loaded with xbuf_frfile()?) into its components
// params: buf         - buffer to parse

//         buflen      - the size in bytes of the input buffer
//         width       - returned image width
//         height      - returned image height
//         palette     - pre-allocated palette (3 * 256 = 768 bytes maximum)
//         nbcolors    - returned nbr of colors
//         transparent - returned color transparent index (-1 if none)
//         comment     - returned allocated GIF comment   ( 0 if none)
// return: pointer on newly allocated bitmap (0 on failure)
// notes:  if 'comment' different from null, you MUST free(comment); but you
//         can pass a null to gif_parse() to say that you don't want comments

u8 *gif_parse(u8 *buf, u32 buflen, u32 *width, u32 *height, u8 *palette,
              u32 *nbcolors, int *transparent, u8 **comment);

// ============================================================================
// Frame buffer
// ----------------------------------------------------------------------------
typedef struct { u8  r,g,b; } rgb_t;
typedef struct { u32 x,y,X,Y; } rect_t;

typedef struct
{
  u8    *bmp,      // bitmap pixels
        *p;        // current cursor position, as a pointer
  int    bbp,      // bits per pixel
         pen, bgd; // current drawing color index / background color index
  rect_t rect;     // used for clipping, windows, etc.
  u32    flags,    // alignment, type of chart, whatever you need
         w, h,     // bitmap width and height
         x, y;     // current cursor position, as row/column coordinates
} bmp_t;           // (used when the 'p' pointer above is null)

// use powers of two for all the flags so we can combine them in 'bmp_t.flags'

enum C_TEXT_STYLE
{
   V_TOP_ALIGN = 1 << 0, // vertical
   V_CEN_ALIGN = 1 << 1,
   V_BOT_ALIGN = 1 << 2,
   H_LEF_ALIGN = 1 << 3, // horizontal
   H_CEN_ALIGN = 1 << 4,
   H_RIG_ALIGN = 1 << 5,
   T_OPAQUE    = 1 << 6  // more flags can be added until 1 << 12:C_CHART_STYLE
};

// create a multi-gradient 'nbcolors'-palette using 'nbsteps' RGB values
// palette   - position in the palette where to store gradient
// nbcolors  - gradient size (in colors)
// steps     - array of rgb_t values used to build the gradient
// nbsteps   - number of entries in the rgb_t values array (must be >= 2)

void dr_gradient(u8 *palette, int nbcolors, rgb_t *steps, int nbsteps);

// img.x/y     - starting point
// img.pen     - color
// img.flags   - alignment
// if alignment is provided then x/y is the left/center/right point used to
// align the text
// (if the full-path 'font' is null then "./fonts/9pts.gif" is used)
// return the lenght of the printed text in pixels
// (clipping is done only on the right side of the frame-buffer)

u32  dr_text  (bmp_t *img, u8 *font, const char *fmt, ...);

// img.pen  - color

void dr_line  (bmp_t *img, int x1, int y1, int x2, int y2);

// img.pen  - color
// if(img.bgd > 0), the circle is filled with the img.bgd color palette index

void dr_circle(bmp_t *img, int x, int y, int radius);

// ============================================================================
// Charts & sparklines
// ----------------------------------------------------------------------------
// to make a sparkline, use C_LINE without (C_TITLES | C_LABELS | C_AVERAGE)
// float tab[] = {10042, 10098, 10182, 10154, 10160, 10132, 10160, 10146};
// bmp_t img;
// img.w     = 30;
// img.h     = 10;
// img.bbp   =  3; // 1 << 3 = 8 colors
// img.flags = C_LINE | C_GRID; // or img.flags = C_BAR;
// dr_chart(&img, 0, 0, 0, 0, tab, sizeof(tab) / sizeof(float));

// use powers of two for all the flags so we can combine them in 'bmp_t.flags'

enum C_CHART_STYLE
{
   C_LINE    = 1 << 12, // line chart
   C_AREA    = 1 << 13, // fill line/bar/pie/ring/dot charts with 'img.bgd' color
   C_BAR     = 1 << 14, // bar  chart
   C_PIE     = 1 << 15, // pie  chart (prettier with dedicated palette entries)
   C_RING    = 1 << 16, // ring chart (a pie chart with a central hole)
   C_DOT     = 1 << 17, // dot  chart
   C_AVERAGE = 1 << 18, // horizontal dotted line showing the average value
   C_LABELS  = 1 << 19, // make room for (and print) x/y axis ticks and labels
   C_TITLES  = 1 << 20, // make room for (and print) title and sub-title (if any)
   C_GRID    = 1 << 21, // draw vertical and horizontal lines in the background
   C_FGRID   = 1 << 22  // the background grid is filled by alternance
};

// params:
// img      - pointer on a bmp_t bitmap structure
// title    - text printed in black at the upper-left corner of the bitmap
// subtitle - text printed in black at the upper-right corner of the bitmap
// tags     - x axis labels (if null, 'ntag' numbers are printed instead)
// ntag     - number of x axis labels
// val      - values to render as a chart
// nval     - number of values to render as a chart

void dr_chart(bmp_t *img, u8 *title, u8 *subtitle,
              u8  **tags, u32 ntag,
              float *val, u32 nval);

// ============================================================================
// Email
// ----------------------------------------------------------------------------
// the 'error' parameter must point to an allocated buffer sized to the total
// size of headers + email body + attachments (encoded in base64: worst case).
// Using 'total size' * 2 is more than safe as base64 inflates by ~33%.
// See the contact.c example for more details:
typedef struct attach_s
{
   char *name; // file name (the extension is used to find the MIME type)
   char *file; // file contents
   u32  size;  // file size
   u32  errlen;// size of the pre-allocated error buffer (mandatory)
   u32  nbr;   // number of attachments (only the 1st array item is necessary)
} attach_t;

// If you wonder why 'nbr' is reduncdant then this is because the attachment
// feature was added to sendemail() but I did not want prior code using it
// to break because of a new 'attachment' function argument.

typedef struct email_s
{
   char       *text;   // must be first
   const char *tag;    // *tag = "@"; to detect an attachment structure
   attach_t   *attach; // array of attachments
} email_t;

// So the email_t structure must be passed as the 'text' sendemail() argument
// when you want to send an email with attachment(s) (or a body with contents
// using 8-bit data, in which case the 'text' field of the attach_t structure
// can be set to "."). See the email.c example.

int sendemail(char *mail_server,
              char *src_mailaddr, char *dst_mailaddr,
              char *subject, char *text,
              char *auth_user, char *auth_pass, // 'login' auth. only
              char *error); // pre-allocated (and later freed) by caller

int isvalidemailaddr(char *szEmail);

// ============================================================================
// Time
// ----------------------------------------------------------------------------
typedef struct _tm_s // just to make our life easier under MS-Windows
{
  u32 tm_sec;   // seconds after the minute - [0,59]
  u32 tm_min;   // minutes after the hour   - [0,59]
  u32 tm_hour;  // hours since midnight     - [0,23]
  u32 tm_mday;  // day of the month         - [1,31]
  u32 tm_mon;   // months since January     - [0,11]
  u32 tm_year;  // years since 1900
  u32 tm_wday;  // days since Sunday        - [0,6]
  u32 tm_yday;  // days since January 1     - [0,365]
  u32 tm_isdst; // daylight savings time flag
} tm_t;

u32      cycles     (void); // return CPU clock cycles (in-minutes overflow)
u64      cycles64   (void); // return CPU clock cycles (will never overflow)
u64      getns      (void); // EPOCH time in nanoseconds  (1 second/1bn)
u64      getus      (void); // EPOCH time in microseconds (1 second/1m)

u64      getms      (void); // EPOCH time in miliseconds  (1 second/1000)

time_t   s_time     (void); // on Windows, much much faster than time(0);
tm_t    *s_gmtime   (time_t t, tm_t *ts); // those are thread-safe
tm_t    *s_localtime(time_t t, tm_t *ts);
char    *s_asctime  (time_t t, char *buf);

size_t   rfc2time   (char *s); // "Tue, 06 Jan 2009 06:12:20 GMT" => u32
char    *time2rfc   (time_t t, char *buf); // inverse of above operation

// ============================================================================
// Firewall
// ----------------------------------------------------------------------------
int  fw_block(char *addr, int is_ip); // at the moment, 'is_ip' MUST be '1'
int  fw_allow(char *addr, int is_ip); // at the moment, 'is_ip' MUST be '1'
void fw_state(xbuf_t *rules);         // return the current rules

// ============================================================================
// Random numbers
// ----------------------------------------------------------------------------
typedef struct { u32 x[5]; } prnd_t;
typedef struct { u32 x[270340]; } rnd_t;

void sw_init(prnd_t *rnd, u32 seed); // pseudo-random numbers generator
u32  sw_rand(prnd_t *rnd);           // (period: 1 << 158)

void hw_init(rnd_t *rnd); // hardware random numbers generator
u32  hw_rand(rnd_t *rnd); // (cache the context: hw_init() takes time)

// ============================================================================
// Checksums
// ----------------------------------------------------------------------------
// u32 crc = 0; // starting value
// crc = crc_32(data, length, crc);

u32 crc_32  (char *data, u32 len, u32 crc);
u32 adler_32(char *data, u32 len, u32 crc); // adler_32 is slower than crc_32

// ============================================================================
// Hashing
// ----------------------------------------------------------------------------
// u8 dst[16]; // the resulting 128-bit hash
// md5_t ctx;
// md5_init(&ctx);
// int i = 10;
// while(i--)
//    md5_add(&ctx, data[i].ptr, data[i].len);
// md5_end(&ctx, dst);

typedef struct { u8 x[220]; } md5_t;

void md5_init(md5_t *ctx);
void md5_add (md5_t *ctx, u8 *src, int srclen);
void md5_end (md5_t *ctx, u8 *dst);
// a wrapper on all the above MD5 calls
void md5(u8 *input, int ilen, u8 *dst);

// u8 dst[20]; // the resulting 160-bit hash
// sha1_t ctx;
// sha1_init(&ctx);
// int i = 10;
// while(i--)
//    sha1_add(&ctx, data[i].ptr, data[i].len);
// sha1_end(&ctx, dst);

typedef struct { u8 x[220]; } sha1_t;
typedef struct { u8 x[232]; } sha2_t;

void sha1_init(sha1_t *ctx);
void sha1_add (sha1_t *ctx, u8 *src, int srclen);
void sha1_end (sha1_t *ctx, u8 *dst);
// a wrapper on all the above SHA-160 calls
void sha1(u8 *input, int ilen, u8 *dst);

// u8 dst[32]; // the resulting 256-bit hash
// sha2_t ctx;
// sha2_init(&ctx);
// int i = 10;
// while(i--)
//    sha2_add(&ctx, data[i].ptr, data[i].len);
// sha2_end(&ctx, dst);

void sha2_init(sha2_t *ctx);
void sha2_add (sha2_t *ctx, u8 *src, int srclen);
void sha2_end (sha2_t *ctx, u8 *dst);
// a wrapper on all the above SHA-256 calls
void sha2(u8 *input, int ilen, u8 *dst);

// ============================================================================
// Encryption
// ----------------------------------------------------------------------------
// AES is the U.S. NIST FIPS PUB 197 standard (2001) developed by Belgians
// Joan Daemen & Vincent Rijmen and approved by the NSA. Useful to comply.

typedef struct aes_s
{
   u32 rounds;
   u32 *keys;
   u32 buf[68];
} aes_t;

// mode     - values 1:ENCRYPT, 0:DECRYPT
// keylen   - values 128, 192 or 256*
//            (*) AES-128 is faster and safer than AES-256

void aes_init(aes_t *ctx, u32 mode, u8 *key, u32 keylen);

// mode     - values 1:ENCRYPT, 0:DECRYPT
// len      - length in bytes to process
// iv       - initialization vector (modified), declare: u8 iv[16];
// src      - source to encrypt
// dst      - destination (encrypted)
//
// Cipher-Block Chaining (CBC) has been invented by IBM in 1976:
// Each plaintext block is XORed with the previously encrypted block before
// being encrypted. It makes all blocks dependent on all the previous blocks.
// To make the ciphertext unique, an IV must be used for the first block.
// It makes encryption sequential (no parallelization) and the message
// requires padding to match the block size.
// A bit change in a plaintext affects all the ciphertext. A plaintext can
// be recovered from 2 contiguous ciphertext blocks, which makes it possible
// to parallelize decryption.

void aes_enc(aes_t *ctx, u32 mode, u32 len, u8 *iv, u8 *src, u8 *dst);

// ============================================================================
// Compression
// ----------------------------------------------------------------------------
// if(gzip == 1) then we use the 'gzip' format, else we use the 'zlib' format
// (the new 'zlib' format is both slower and larger than the old 'gzip' format)
// if you already have the crc32, pass it into 'crc', else 'crc' MUST be NULL
// return the dstlen, 0 on error
//
// You can pass the compression rate (1-9) by using the 'gzip' argument:
// gzip = (6 << 1) + 1; // (6 << 1):comp. rate 6, "+ 1":Gzip, "+ 0":Deflate

u32 zlib_cmp(char *src, u32 *crc, u32 srclen, char *dst, u32 dstlen, int gzip);

#ifdef __cplusplus
 }
#endif

#endif
// ============================================================================
// End of Source Code
// ============================================================================

