/* src/modules/common/AppDefines.h.  Generated from AppDefines.h.in by configure.  */
/* src/modules/common/AppDefines.h.in.  Generated from configure.ac by autoheader.  */

/* set configure mode */
#define BUILT_WITH_CONFGURE 1

/* set DEBUG mode */
/* #undef DEBUG */

/* Define to 1 if you have the <arpa/inet.h> header file. */
#define HAVE_ARPA_INET_H 1

/* Define to 1 if you have the `clock_gettime' function. */
#define HAVE_CLOCK_GETTIME 1

/* Define if you have clock_nanosleep() */
#define HAVE_CLOCK_NANOSLEEP 1

/* define if the compiler supports basic C++11 syntax */
/* #undef HAVE_CXX11 */

/* Define to 1 if you have the <dlfcn.h> header file. */
#define HAVE_DLFCN_H 1

/* Define to 1 if you have the <fcntl.h> header file. */
#define HAVE_FCNTL_H 1

/* Define to 1 if you have the `floor' function. */
/* #undef HAVE_FLOOR */

/* Define to 1 if you have the `fork' function. */
#define HAVE_FORK 1

/* Define to 1 if you have the `gethostbyname' function. */
#define HAVE_GETHOSTBYNAME 1

/* Define to 1 if you have the <inttypes.h> header file. */
#define HAVE_INTTYPES_H 1

/* Define to 1 if you have the `bson-1.0' library (-lbson-1.0). */
#define HAVE_LIBBSON_1_0 1

/* Define to 1 if you have the `crypto' library (-lcrypto). */
#define HAVE_LIBCRYPTO 1

/* Define to 1 if you have the `gtmshr' library (-lgtmshr). */
/* #undef HAVE_LIBGTMSHR */

/* Define to 1 if you have the `kernel32' library (-lkernel32). */
/* #undef HAVE_LIBKERNEL32 */

/* Define to 1 if you have the `mongoc-1.0' library (-lmongoc-1.0). */
#define HAVE_LIBMONGOC_1_0 1

/* Define to 1 if you have the `odbc' library (-lodbc). */
/* #undef HAVE_LIBODBC */

/* Define to 1 if you have the `pthread' library (-lpthread). */
#define HAVE_LIBPTHREAD 1

/* Define to 1 if you have the `pthreads' library (-lpthreads). */
/* #undef HAVE_LIBPTHREADS */

/* Define to 1 if you have the `regex' library (-lregex). */
/* #undef HAVE_LIBREGEX */

/* Define to 1 if you have the `rt' library (-lrt). */
#define HAVE_LIBRT 1

/* Define to 1 if you have the `ssl' library (-lssl). */
#define HAVE_LIBSSL 1

/* Define to 1 if you have the `uuid' library (-luuid). */
#define HAVE_LIBUUID 1

/* Define to 1 if you have the `ws2_32' library (-lws2_32). */
/* #undef HAVE_LIBWS2_32 */

/* Define to 1 if you have the `wsock32' library (-lwsock32). */
/* #undef HAVE_LIBWSOCK32 */

/* Define to 1 if you have the `z' library (-lz). */
#define HAVE_LIBZ 1

/* Define to 1 if the system has the type `long long'. */
#define HAVE_LONG_LONG 1

/* Define to 1 if your system has a GNU libc compatible `malloc' function, and
   to 0 otherwise. */
#define HAVE_MALLOC 1

/* Define to 1 if you have the <memory.h> header file. */
#define HAVE_MEMORY_H 1

/* Define to 1 if you have the `memset' function. */
#define HAVE_MEMSET 1

/* Define to 1 if you have the <netdb.h> header file. */
#define HAVE_NETDB_H 1

/* Define to 1 if you have the <netinet/in.h> header file. */
#define HAVE_NETINET_IN_H 1

/* Define to 1 if you have the `pow' function. */
/* #undef HAVE_POW */

/* Define to 1 if you have the `regcomp' function. */
#define HAVE_REGCOMP 1

/* Define to 1 if you have the `select' function. */
#define HAVE_SELECT 1

/* Define to 1 if the system has the type `signed char'. */
#define HAVE_SIGNED_CHAR 1

/* Define to 1 if you have the `socket' function. */
#define HAVE_SOCKET 1

/* Define to 1 if you have the <stdint.h> header file. */
#define HAVE_STDINT_H 1

/* Define to 1 if you have the <stdlib.h> header file. */
#define HAVE_STDLIB_H 1

/* Define to 1 if you have the `strcasecmp' function. */
#define HAVE_STRCASECMP 1

/* Define to 1 if you have the <strings.h> header file. */
#define HAVE_STRINGS_H 1

/* Define to 1 if you have the <string.h> header file. */
#define HAVE_STRING_H 1

/* Define to 1 if you have the `strtol' function. */
#define HAVE_STRTOL 1

/* Define to 1 if you have the `strtoul' function. */
#define HAVE_STRTOUL 1

/* Define to 1 if you have the <sys/ioctl.h> header file. */
#define HAVE_SYS_IOCTL_H 1

/* Define to 1 if you have the <sys/socket.h> header file. */
#define HAVE_SYS_SOCKET_H 1

/* Define to 1 if you have the <sys/stat.h> header file. */
#define HAVE_SYS_STAT_H 1

/* Define to 1 if you have the <sys/time.h> header file. */
#define HAVE_SYS_TIME_H 1

/* Define to 1 if you have the <sys/types.h> header file. */
#define HAVE_SYS_TYPES_H 1

/* Define to 1 if you have the <unistd.h> header file. */
#define HAVE_UNISTD_H 1

/* Define to 1 if you have the `vfork' function. */
#define HAVE_VFORK 1

/* Define to 1 if you have the <vfork.h> header file. */
/* #undef HAVE_VFORK_H */

/* Define to 1 if `fork' works. */
#define HAVE_WORKING_FORK 1

/* Define to 1 if `vfork' works. */
#define HAVE_WORKING_VFORK 1

/* enable http framework application flow module */
#define INC_APPFLOW /**/

/* enable binary serilaization module */
#define INC_BINSER /**/

/* enable client-utility module */
#define INC_CLIENTUTIL /**/

/* enable component module */
/* #undef INC_CMP */

/* enable http framework dynamic c++ pages module */
#define INC_DCP /**/

/* enable distocache module */
/* #undef INC_DSTC */

/* enable http framework dynamic views module */
#define INC_DVIEW /**/

/* enable sdorm gtm module */
/* #undef INC_GTM */

/* enable http module */
#define INC_HTTP /**/

/* enable http framework module */
#define INC_HTTPFRAMEWORK /**/

/* enable c++ interpreter module */
#define INC_INTP /**/

/* enable ioc module */
#define INC_IOC /**/

/* enable cron/batch jobs module */
#define INC_JOBS /**/

/* enable json serilaization module */
#define INC_JSONSER /**/

/* enable memcached cache module */
/* #undef INC_MEMCACHED */

/* enable memory cache module */
#define INC_MEMORYCACHE /**/

/* enable method invoker module */
/* #undef INC_MI */

/* enable message handler module */
/* #undef INC_MSGH */

/* enable redis cache module */
/* #undef INC_REDISCACHE */

/* enable reflection module */
#define INC_REFLECTION /**/

/* enable http framework scripting support module */
#define INC_SCRH /**/

/* enable data-source-orm module */
#define INC_SDORM /**/

/* enable data-source-orm-mongo module */
#define INC_SDORM_MONGO /**/

/* enable data-source-orm-sql module */
/* #undef INC_SDORM_SQL */

/* enable server-utility module */
#define INC_SERVERUTIL /**/

/* enable ssl-utility module */
#define INC_SSLUTIL /**/

/* enable threads module */
#define INC_THREADS /**/

/* enable http framework templates module */
#define INC_TPE /**/

/* enable http framework web-service module */
#define INC_WEBSVC /**/

/* enable xml serialization module */
#define INC_XMLSER /**/

/* Define to the sub-directory in which libtool stores uninstalled libraries.
   */
#define LT_OBJDIR ".libs/"

/* unset DEBUG mode */
#define NDEBUG /**/

/* set OS to aix */
/* #undef OS_AIX */

/* set OS to bsd */
/* #undef OS_BSD */

/* set OS to cygwin */
/* #undef OS_CYGWIN */

/* set OS to darwin */
/* #undef OS_DARWIN */

/* set OS to freebsd */
/* #undef OS_FREEBSD */

/* set OS to hp-ux */
/* #undef OS_HPUX */

/* set OS to irix */
/* #undef OS_IRIX */

/* set OS to linux */
#define OS_LINUX /**/

/* set OS to mingw */
/* #undef OS_MINGW */

/* Define to 0 if you don't have _mingw_mac.h */
#define OS_MINGW_W64 0

/* set OS to netbsd */
/* #undef OS_NETBSD */

/* set OS to openbsd */
/* #undef OS_OPENBSD */

/* set openbsd release number */
/* #undef OS_OPENBSD_REL */

/* set OS to osf */
/* #undef OS_OSF */

/* set OS to sco */
/* #undef OS_SCO */

/* set OS to solaris */
/* #undef OS_SOLARIS */

/* set OS to sunos */
/* #undef OS_SUNOS */

/* set OS to svr4 */
/* #undef OS_SVR4 */

/* Define to the address where bug reports for this package should be sent. */
#define PACKAGE_BUGREPORT "sumeet.chhetri@gmail.com"

/* Define to the full name of this package. */
#define PACKAGE_NAME "ffead-cpp"

/* Define to the full name and version of this package. */
#define PACKAGE_STRING "ffead-cpp 2.0"

/* Define to the one symbol short name of this package. */
#define PACKAGE_TARNAME "ffead-cpp"

/* Define to the home page for this package. */
#define PACKAGE_URL ""

/* Define to the version of this package. */
#define PACKAGE_VERSION "2.0"

/* The size of `long', as computed by sizeof. */
#define SIZEOF_LONG 8

/* The size of `void *', as computed by sizeof. */
#define SIZEOF_VOID_P 8

/* Define to 1 if you have the ANSI C header files. */
#define STDC_HEADERS 1

/* devpoll support not found */
#define USE_DEVPOLL 0

/* epoll support not found */
#define USE_EPOLL 1

/* event port support not found */
#define USE_EVPORT 0

/* kqueue support not found */
#define USE_KQUEUE 0

/* poll support not found */
#define USE_POLL 1

/* select support not found */
#define USE_SELECT 1

/* use io completion ports on windows */
/* #undef USE_WIN_IOCP */

/* Define to rpl_malloc if the replacement function should be used. */
/* #undef malloc */

/* Define to `int' if <sys/types.h> does not define. */
/* #undef pid_t */

/* Define to `unsigned int' if <sys/types.h> does not define. */
/* #undef size_t */

/* Define as `fork' if `vfork' does not work. */
/* #undef vfork */
