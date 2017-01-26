/* This file was automatically generated.  Do not edit (Except for compile time directives)! */ 
#ifndef _UNQLITE_H_
#define _UNQLITE_H_
/*
 * Symisc UnQLite: An Embeddable NoSQL (Post Modern) Database Engine.
 * Copyright (C) 2012-2013, Symisc Systems http://unqlite.org/
 * Version 1.1.6
 * For information on licensing, redistribution of this file, and for a DISCLAIMER OF ALL WARRANTIES
 * please contact Symisc Systems via:
 *       legal@symisc.net
 *       licensing@symisc.net
 *       contact@symisc.net
 * or visit:
 *      http://unqlite.org/licensing.html
 */
/*
 * Copyright (C) 2012, 2013 Symisc Systems, S.U.A.R.L [M.I.A.G Mrad Chems Eddine <chm@symisc.net>].
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY SYMISC SYSTEMS ``AS IS'' AND ANY EXPRESS
 * OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR
 * NON-INFRINGEMENT, ARE DISCLAIMED.  IN NO EVENT SHALL SYMISC SYSTEMS
 * BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
 * BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, 
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
 * OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
 * IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
 /* $SymiscID: unqlite.h v1.1 UNIX|WIN32/64 2012-11-02 02:10 stable <chm@symisc.net> $ */
#include <stdarg.h> /* needed for the definition of va_list */
/*
 * Compile time engine version, signature, identification in the symisc source tree
 * and copyright notice.
 * Each macro have an equivalent C interface associated with it that provide the same
 * information but are associated with the library instead of the header file.
 * Refer to [unqlite_lib_version()], [unqlite_lib_signature()], [unqlite_lib_ident()] and
 * [unqlite_lib_copyright()] for more information.
 */
/*
 * The UNQLITE_VERSION C preprocessor macroevaluates to a string literal
 * that is the unqlite version in the format "X.Y.Z" where X is the major
 * version number and Y is the minor version number and Z is the release
 * number.
 */
#define UNQLITE_VERSION "1.1.6"
/*
 * The UNQLITE_VERSION_NUMBER C preprocessor macro resolves to an integer
 * with the value (X*1000000 + Y*1000 + Z) where X, Y, and Z are the same
 * numbers used in [UNQLITE_VERSION].
 */
#define UNQLITE_VERSION_NUMBER 1001006
/*
 * The UNQLITE_SIG C preprocessor macro evaluates to a string
 * literal which is the public signature of the unqlite engine.
 * This signature could be included for example in a host-application
 * generated Server MIME header as follows:
 *   Server: YourWebServer/x.x unqlite/x.x.x \r\n
 */
#define UNQLITE_SIG "unqlite/1.1.6"
/*
 * UnQLite identification in the Symisc source tree:
 * Each particular check-in of a particular software released
 * by symisc systems have an unique identifier associated with it.
 * This macro hold the one associated with unqlite.
 */
#define UNQLITE_IDENT "unqlite:b172a1e2c3f62fb35c8e1fb2795121f82356cad6"
/*
 * Copyright notice.
 * If you have any questions about the licensing situation, please
 * visit http://unqlite.org/licensing.html
 * or contact Symisc Systems via:
 *   legal@symisc.net
 *   licensing@symisc.net
 *   contact@symisc.net
 */
#define UNQLITE_COPYRIGHT "Copyright (C) Symisc Systems, S.U.A.R.L [Mrad Chems Eddine <chm@symisc.net>] 2012-2013, http://unqlite.org/"

/* Forward declaration to public objects */
typedef struct unqlite_io_methods unqlite_io_methods;
typedef struct unqlite_kv_methods unqlite_kv_methods;
typedef struct unqlite_kv_engine unqlite_kv_engine;
typedef struct jx9_io_stream unqlite_io_stream;
typedef struct jx9_context unqlite_context;
typedef struct jx9_value unqlite_value;
typedef struct unqlite_vfs unqlite_vfs;
typedef struct unqlite_vm unqlite_vm;
typedef struct unqlite unqlite;
/*
 * ------------------------------
 * Compile time directives
 * ------------------------------
 * For most purposes, UnQLite can be built just fine using the default compilation options.
 * However, if required, the compile-time options documented below can be used to omit UnQLite
 * features (resulting in a smaller compiled library size) or to change the default values
 * of some parameters.
 * Every effort has been made to ensure that the various combinations of compilation options
 * work harmoniously and produce a working library.
 *
 * UNQLITE_ENABLE_THREADS
 *  This option controls whether or not code is included in UnQLite to enable it to operate
 *  safely in a multithreaded environment. The default is not. All mutexing code is omitted
 *  and it is unsafe to use UnQLite in a multithreaded program. When compiled with the
 *  UNQLITE_ENABLE_THREADS directive enabled, UnQLite can be used in a multithreaded program
 *  and it is safe to share the same virtual machine and engine handle between two or more threads.
 *  The value of UNQLITE_ENABLE_THREADS can be determined at run-time using the unqlite_lib_is_threadsafe()
 *  interface.
 *  When UnQLite has been compiled with threading support then the threading mode can be altered
 * at run-time using the unqlite_lib_config() interface together with one of these verbs:
 *    UNQLITE_LIB_CONFIG_THREAD_LEVEL_SINGLE
 *    UNQLITE_LIB_CONFIG_THREAD_LEVEL_MULTI
 *  Platforms others than Windows and UNIX systems must install their own mutex subsystem via 
 *  unqlite_lib_config() with a configuration verb set to UNQLITE_LIB_CONFIG_USER_MUTEX.
 *  Otherwise the library is not threadsafe.
 *  Note that you must link UnQLite with the POSIX threads library under UNIX systems (i.e: -lpthread).
 *
 * Options To Omit/Enable Features
 *
 * The following options can be used to reduce the size of the compiled library by omitting optional
 * features. This is probably only useful in embedded systems where space is especially tight, as even
 * with all features included the UnQLite library is relatively small. Don't forget to tell your
 * compiler to optimize for binary size! (the -Os option if using GCC). Telling your compiler
 * to optimize for size usually has a much larger impact on library footprint than employing
 * any of these compile-time options.
 *
 * JX9_DISABLE_BUILTIN_FUNC
 *  Jx9 is shipped with more than 312 built-in functions suitable for most purposes like 
 *  string and INI processing, ZIP extracting, Base64 encoding/decoding, JSON encoding/decoding
 *  and so forth.
 *  If this directive is enabled, then all built-in Jx9 functions are omitted from the build.
 *  Note that special functions such as db_create(), db_store(), db_fetch(), etc. are not omitted
 *  from the build and are not affected by this directive.
 *
 * JX9_ENABLE_MATH_FUNC
 *  If this directive is enabled, built-in math functions such as sqrt(), abs(), log(), ceil(), etc.
 *  are included in the build. Note that you may need to link UnQLite with the math library in same
 *  Linux/BSD flavor (i.e: -lm).
 *
 * JX9_DISABLE_DISK_IO
 *  If this directive is enabled, built-in VFS functions such as chdir(), mkdir(), chroot(), unlink(),
 *  sleep(), etc. are omitted from the build.
 *
 * UNQLITE_ENABLE_JX9_HASH_IO
 * If this directive is enabled, built-in hash functions such as md5(), sha1(), md5_file(), crc32(), etc.
 * are included in the build.
 */
/* Symisc public definitions */
#if !defined(SYMISC_STANDARD_DEFS)
#define SYMISC_STANDARD_DEFS
#if defined (_WIN32) || defined (WIN32) || defined(__MINGW32__) || defined (_MSC_VER) || defined (_WIN32_WCE)
/* Windows Systems */
#if !defined(__WINNT__)
#define __WINNT__
#endif 
/*
 * Determine if we are dealing with WindowsCE - which has a much
 * reduced API.
 */
#if defined(_WIN32_WCE)
#ifndef __WIN_CE__
#define __WIN_CE__
#endif /* __WIN_CE__ */
#endif /* _WIN32_WCE */
#else
/*
 * By default we will assume that we are compiling on a UNIX systems.
 * Otherwise the OS_OTHER directive must be defined.
 */
#if !defined(OS_OTHER)
#if !defined(__UNIXES__)
#define __UNIXES__
#endif /* __UNIXES__ */
#else
#endif /* OS_OTHER */
#endif /* __WINNT__/__UNIXES__ */
#if defined(_MSC_VER) || defined(__BORLANDC__)
typedef signed __int64     sxi64; /* 64 bits(8 bytes) signed int64 */
typedef unsigned __int64   sxu64; /* 64 bits(8 bytes) unsigned int64 */
#else
typedef signed long long int   sxi64; /* 64 bits(8 bytes) signed int64 */
typedef unsigned long long int sxu64; /* 64 bits(8 bytes) unsigned int64 */
#endif /* _MSC_VER */
/* Signature of the consumer routine */
typedef int (*ProcConsumer)(const void *, unsigned int, void *);
/* Forward reference */
typedef struct SyMutexMethods SyMutexMethods;
typedef struct SyMemMethods SyMemMethods;
typedef struct SyString SyString;
typedef struct syiovec syiovec;
typedef struct SyMutex SyMutex;
typedef struct Sytm Sytm;
/* Scatter and gather array. */
struct syiovec
{
#if defined (__WINNT__)
	/* Same fields type and offset as WSABUF structure defined one winsock2 header */
	unsigned long nLen;
	char *pBase;
#else
	void *pBase;
	unsigned long nLen;
#endif
};
struct SyString
{
	const char *zString;  /* Raw string (may not be null terminated) */
	unsigned int nByte;   /* Raw string length */
};
/* Time structure. */
struct Sytm
{
  int tm_sec;     /* seconds (0 - 60) */
  int tm_min;     /* minutes (0 - 59) */
  int tm_hour;    /* hours (0 - 23) */
  int tm_mday;    /* day of month (1 - 31) */
  int tm_mon;     /* month of year (0 - 11) */
  int tm_year;    /* year + 1900 */
  int tm_wday;    /* day of week (Sunday = 0) */
  int tm_yday;    /* day of year (0 - 365) */
  int tm_isdst;   /* is summer time in effect? */
  char *tm_zone;  /* abbreviation of timezone name */
  long tm_gmtoff; /* offset from UTC in seconds */
};
/* Convert a tm structure (struct tm *) found in <time.h> to a Sytm structure */
#define STRUCT_TM_TO_SYTM(pTM, pSYTM) \
	(pSYTM)->tm_hour = (pTM)->tm_hour;\
	(pSYTM)->tm_min	 = (pTM)->tm_min;\
	(pSYTM)->tm_sec	 = (pTM)->tm_sec;\
	(pSYTM)->tm_mon	 = (pTM)->tm_mon;\
	(pSYTM)->tm_mday = (pTM)->tm_mday;\
	(pSYTM)->tm_year = (pTM)->tm_year + 1900;\
	(pSYTM)->tm_yday = (pTM)->tm_yday;\
	(pSYTM)->tm_wday = (pTM)->tm_wday;\
	(pSYTM)->tm_isdst = (pTM)->tm_isdst;\
	(pSYTM)->tm_gmtoff = 0;\
	(pSYTM)->tm_zone = 0;

/* Convert a SYSTEMTIME structure (LPSYSTEMTIME: Windows Systems only ) to a Sytm structure */
#define SYSTEMTIME_TO_SYTM(pSYSTIME, pSYTM) \
	 (pSYTM)->tm_hour = (pSYSTIME)->wHour;\
	 (pSYTM)->tm_min  = (pSYSTIME)->wMinute;\
	 (pSYTM)->tm_sec  = (pSYSTIME)->wSecond;\
	 (pSYTM)->tm_mon  = (pSYSTIME)->wMonth - 1;\
	 (pSYTM)->tm_mday = (pSYSTIME)->wDay;\
	 (pSYTM)->tm_year = (pSYSTIME)->wYear;\
	 (pSYTM)->tm_yday = 0;\
	 (pSYTM)->tm_wday = (pSYSTIME)->wDayOfWeek;\
	 (pSYTM)->tm_gmtoff = 0;\
	 (pSYTM)->tm_isdst = -1;\
	 (pSYTM)->tm_zone = 0;

/* Dynamic memory allocation methods. */
struct SyMemMethods 
{
	void * (*xAlloc)(unsigned int);          /* [Required:] Allocate a memory chunk */
	void * (*xRealloc)(void *, unsigned int); /* [Required:] Re-allocate a memory chunk */
	void   (*xFree)(void *);                 /* [Required:] Release a memory chunk */
	unsigned int  (*xChunkSize)(void *);     /* [Optional:] Return chunk size */
	int    (*xInit)(void *);                 /* [Optional:] Initialization callback */
	void   (*xRelease)(void *);              /* [Optional:] Release callback */
	void  *pUserData;                        /* [Optional:] First argument to xInit() and xRelease() */
};
/* Out of memory callback signature. */
typedef int (*ProcMemError)(void *);
/* Mutex methods. */
struct SyMutexMethods 
{
	int (*xGlobalInit)(void);		/* [Optional:] Global mutex initialization */
	void  (*xGlobalRelease)(void);	/* [Optional:] Global Release callback () */
	SyMutex * (*xNew)(int);	        /* [Required:] Request a new mutex */
	void  (*xRelease)(SyMutex *);	/* [Optional:] Release a mutex  */
	void  (*xEnter)(SyMutex *);	    /* [Required:] Enter mutex */
	int (*xTryEnter)(SyMutex *);    /* [Optional:] Try to enter a mutex */
	void  (*xLeave)(SyMutex *);	    /* [Required:] Leave a locked mutex */
};
#if defined (_MSC_VER) || defined (__MINGW32__) ||  defined (__GNUC__) && defined (__declspec)
#define SX_APIIMPORT	__declspec(dllimport)
#define SX_APIEXPORT	__declspec(dllexport)
#else
#define	SX_APIIMPORT
#define	SX_APIEXPORT
#endif
/* Standard return values from Symisc public interfaces */
#define SXRET_OK       0      /* Not an error */	
#define SXERR_MEM      (-1)   /* Out of memory */
#define SXERR_IO       (-2)   /* IO error */
#define SXERR_EMPTY    (-3)   /* Empty field */
#define SXERR_LOCKED   (-4)   /* Locked operation */
#define SXERR_ORANGE   (-5)   /* Out of range value */
#define SXERR_NOTFOUND (-6)   /* Item not found */
#define SXERR_LIMIT    (-7)   /* Limit reached */
#define SXERR_MORE     (-8)   /* Need more input */
#define SXERR_INVALID  (-9)   /* Invalid parameter */
#define SXERR_ABORT    (-10)  /* User callback request an operation abort */
#define SXERR_EXISTS   (-11)  /* Item exists */
#define SXERR_SYNTAX   (-12)  /* Syntax error */
#define SXERR_UNKNOWN  (-13)  /* Unknown error */
#define SXERR_BUSY     (-14)  /* Busy operation */
#define SXERR_OVERFLOW (-15)  /* Stack or buffer overflow */
#define SXERR_WILLBLOCK (-16) /* Operation will block */
#define SXERR_NOTIMPLEMENTED  (-17) /* Operation not implemented */
#define SXERR_EOF      (-18) /* End of input */
#define SXERR_PERM     (-19) /* Permission error */
#define SXERR_NOOP     (-20) /* No-op */	
#define SXERR_FORMAT   (-21) /* Invalid format */
#define SXERR_NEXT     (-22) /* Not an error */
#define SXERR_OS       (-23) /* System call return an error */
#define SXERR_CORRUPT  (-24) /* Corrupted pointer */
#define SXERR_CONTINUE (-25) /* Not an error: Operation in progress */
#define SXERR_NOMATCH  (-26) /* No match */
#define SXERR_RESET    (-27) /* Operation reset */
#define SXERR_DONE     (-28) /* Not an error */
#define SXERR_SHORT    (-29) /* Buffer too short */
#define SXERR_PATH     (-30) /* Path error */
#define SXERR_TIMEOUT  (-31) /* Timeout */
#define SXERR_BIG      (-32) /* Too big for processing */
#define SXERR_RETRY    (-33) /* Retry your call */
#define SXERR_IGNORE   (-63) /* Ignore */
#endif /* SYMISC_PUBLIC_DEFS */
/* 
 * Marker for exported interfaces. 
 */
#define UNQLITE_APIEXPORT SX_APIEXPORT
/*
 * If compiling for a processor that lacks floating point
 * support, substitute integer for floating-point.
 */
#ifdef UNQLITE_OMIT_FLOATING_POINT
typedef sxi64 uqlite_real;
#else
typedef double unqlite_real;
#endif
typedef sxi64 unqlite_int64;
/* Standard UnQLite return values */
#define UNQLITE_OK      SXRET_OK      /* Successful result */
/* Beginning of error codes */
#define UNQLITE_NOMEM    SXERR_MEM     /* Out of memory */
#define UNQLITE_ABORT    SXERR_ABORT   /* Another thread have released this instance */
#define UNQLITE_IOERR    SXERR_IO      /* IO error */
#define UNQLITE_CORRUPT  SXERR_CORRUPT /* Corrupt pointer */
#define UNQLITE_LOCKED   SXERR_LOCKED  /* Forbidden Operation */ 
#define UNQLITE_BUSY	 SXERR_BUSY    /* The database file is locked */
#define UNQLITE_DONE	 SXERR_DONE    /* Operation done */
#define UNQLITE_PERM     SXERR_PERM    /* Permission error */
#define UNQLITE_NOTIMPLEMENTED SXERR_NOTIMPLEMENTED /* Method not implemented by the underlying Key/Value storage engine */
#define UNQLITE_NOTFOUND SXERR_NOTFOUND /* No such record */
#define UNQLITE_NOOP     SXERR_NOOP     /* No such method */
#define UNQLITE_INVALID  SXERR_INVALID  /* Invalid parameter */
#define UNQLITE_EOF      SXERR_EOF      /* End Of Input */
#define UNQLITE_UNKNOWN  SXERR_UNKNOWN  /* Unknown configuration option */
#define UNQLITE_LIMIT    SXERR_LIMIT    /* Database limit reached */
#define UNQLITE_EXISTS   SXERR_EXISTS   /* Record exists */
#define UNQLITE_EMPTY    SXERR_EMPTY    /* Empty record */
#define UNQLITE_COMPILE_ERR (-70)       /* Compilation error */
#define UNQLITE_VM_ERR      (-71)       /* Virtual machine error */
#define UNQLITE_FULL        (-73)       /* Full database (unlikely) */
#define UNQLITE_CANTOPEN    (-74)       /* Unable to open the database file */
#define UNQLITE_READ_ONLY   (-75)       /* Read only Key/Value storage engine */
#define UNQLITE_LOCKERR     (-76)       /* Locking protocol error */
/* end-of-error-codes */
/*
 * Database Handle Configuration Commands.
 *
 * The following set of constants are the available configuration verbs that can
 * be used by the host-application to configure an UnQLite database handle.
 * These constants must be passed as the second argument to [unqlite_config()].
 *
 * Each options require a variable number of arguments.
 * The [unqlite_config()] interface will return UNQLITE_OK on success, any other
 * return value indicates failure.
 * For a full discussion on the configuration verbs and their expected 
 * parameters, please refer to this page:
 *      http://unqlite.org/c_api/unqlite_config.html
 */
#define UNQLITE_CONFIG_JX9_ERR_LOG         1  /* TWO ARGUMENTS: const char **pzBuf, int *pLen */
#define UNQLITE_CONFIG_MAX_PAGE_CACHE      2  /* ONE ARGUMENT: int nMaxPage */
#define UNQLITE_CONFIG_ERR_LOG             3  /* TWO ARGUMENTS: const char **pzBuf, int *pLen */
#define UNQLITE_CONFIG_KV_ENGINE           4  /* ONE ARGUMENT: const char *zKvName */
#define UNQLITE_CONFIG_DISABLE_AUTO_COMMIT 5  /* NO ARGUMENTS */
#define UNQLITE_CONFIG_GET_KV_NAME         6  /* ONE ARGUMENT: const char **pzPtr */
/*
 * UnQLite/Jx9 Virtual Machine Configuration Commands.
 *
 * The following set of constants are the available configuration verbs that can
 * be used by the host-application to configure the Jx9 (Via UnQLite) Virtual machine.
 * These constants must be passed as the second argument to the [unqlite_vm_config()] 
 * interface.
 * Each options require a variable number of arguments.
 * The [unqlite_vm_config()] interface will return UNQLITE_OK on success, any other return
 * value indicates failure.
 * There are many options but the most importants are: UNQLITE_VM_CONFIG_OUTPUT which install
 * a VM output consumer callback, UNQLITE_VM_CONFIG_HTTP_REQUEST which parse and register
 * a HTTP request and UNQLITE_VM_CONFIG_ARGV_ENTRY which populate the $argv array.
 * For a full discussion on the configuration verbs and their expected parameters, please
 * refer to this page:
 *      http://unqlite.org/c_api/unqlite_vm_config.html
 */
#define UNQLITE_VM_CONFIG_OUTPUT           1  /* TWO ARGUMENTS: int (*xConsumer)(const void *pOut, unsigned int nLen, void *pUserData), void *pUserData */
#define UNQLITE_VM_CONFIG_IMPORT_PATH      2  /* ONE ARGUMENT: const char *zIncludePath */
#define UNQLITE_VM_CONFIG_ERR_REPORT       3  /* NO ARGUMENTS: Report all run-time errors in the VM output */
#define UNQLITE_VM_CONFIG_RECURSION_DEPTH  4  /* ONE ARGUMENT: int nMaxDepth */
#define UNQLITE_VM_OUTPUT_LENGTH           5  /* ONE ARGUMENT: unsigned int *pLength */
#define UNQLITE_VM_CONFIG_CREATE_VAR       6  /* TWO ARGUMENTS: const char *zName, unqlite_value *pValue */
#define UNQLITE_VM_CONFIG_HTTP_REQUEST     7  /* TWO ARGUMENTS: const char *zRawRequest, int nRequestLength */
#define UNQLITE_VM_CONFIG_SERVER_ATTR      8  /* THREE ARGUMENTS: const char *zKey, const char *zValue, int nLen */
#define UNQLITE_VM_CONFIG_ENV_ATTR         9  /* THREE ARGUMENTS: const char *zKey, const char *zValue, int nLen */
#define UNQLITE_VM_CONFIG_EXEC_VALUE      10  /* ONE ARGUMENT: unqlite_value **ppValue */
#define UNQLITE_VM_CONFIG_IO_STREAM       11  /* ONE ARGUMENT: const unqlite_io_stream *pStream */
#define UNQLITE_VM_CONFIG_ARGV_ENTRY      12  /* ONE ARGUMENT: const char *zValue */
#define UNQLITE_VM_CONFIG_EXTRACT_OUTPUT  13  /* TWO ARGUMENTS: const void **ppOut, unsigned int *pOutputLen */
/*
 * Storage engine configuration commands.
 *
 * The following set of constants are the available configuration verbs that can
 * be used by the host-application to configure the underlying storage engine (i.e Hash, B+tree, R+tree).
 * These constants must be passed as the first argument to [unqlite_kv_config()].
 * Each options require a variable number of arguments.
 * The [unqlite_kv_config()] interface will return UNQLITE_OK on success, any other return
 * value indicates failure.
 * For a full discussion on the configuration verbs and their expected parameters, please
 * refer to this page:
 *      http://unqlite.org/c_api/unqlite_kv_config.html
 */
#define UNQLITE_KV_CONFIG_HASH_FUNC  1 /* ONE ARGUMENT: unsigned int (*xHash)(const void *,unsigned int) */
#define UNQLITE_KV_CONFIG_CMP_FUNC   2 /* ONE ARGUMENT: int (*xCmp)(const void *,const void *,unsigned int) */
/*
 * Global Library Configuration Commands.
 *
 * The following set of constants are the available configuration verbs that can
 * be used by the host-application to configure the whole library.
 * These constants must be passed as the first argument to [unqlite_lib_config()].
 *
 * Each options require a variable number of arguments.
 * The [unqlite_lib_config()] interface will return UNQLITE_OK on success, any other return
 * value indicates failure.
 * Notes:
 * The default configuration is recommended for most applications and so the call to
 * [unqlite_lib_config()] is usually not necessary. It is provided to support rare 
 * applications with unusual needs. 
 * The [unqlite_lib_config()] interface is not threadsafe. The application must insure that
 * no other [unqlite_*()] interfaces are invoked by other threads while [unqlite_lib_config()]
 * is running. Furthermore, [unqlite_lib_config()] may only be invoked prior to library
 * initialization using [unqlite_lib_init()] or [unqlite_init()] or after shutdown
 * by [unqlite_lib_shutdown()]. If [unqlite_lib_config()] is called after [unqlite_lib_init()]
 * or [unqlite_init()] and before [unqlite_lib_shutdown()] then it will return UNQLITE_LOCKED.
 * For a full discussion on the configuration verbs and their expected parameters, please
 * refer to this page:
 *      http://unqlite.org/c_api/unqlite_lib.html
 */
#define UNQLITE_LIB_CONFIG_USER_MALLOC            1 /* ONE ARGUMENT: const SyMemMethods *pMemMethods */ 
#define UNQLITE_LIB_CONFIG_MEM_ERR_CALLBACK       2 /* TWO ARGUMENTS: int (*xMemError)(void *), void *pUserData */
#define UNQLITE_LIB_CONFIG_USER_MUTEX             3 /* ONE ARGUMENT: const SyMutexMethods *pMutexMethods */ 
#define UNQLITE_LIB_CONFIG_THREAD_LEVEL_SINGLE    4 /* NO ARGUMENTS */ 
#define UNQLITE_LIB_CONFIG_THREAD_LEVEL_MULTI     5 /* NO ARGUMENTS */ 
#define UNQLITE_LIB_CONFIG_VFS                    6 /* ONE ARGUMENT: const unqlite_vfs *pVfs */
#define UNQLITE_LIB_CONFIG_STORAGE_ENGINE         7 /* ONE ARGUMENT: unqlite_kv_methods *pStorage */
#define UNQLITE_LIB_CONFIG_PAGE_SIZE              8 /* ONE ARGUMENT: int iPageSize */
/*
 * These bit values are intended for use in the 3rd parameter to the [unqlite_open()] interface
 * and in the 4th parameter to the xOpen method of the [unqlite_vfs] object.
 */
#define UNQLITE_OPEN_READONLY         0x00000001  /* Read only mode. Ok for [unqlite_open] */
#define UNQLITE_OPEN_READWRITE        0x00000002  /* Ok for [unqlite_open] */
#define UNQLITE_OPEN_CREATE           0x00000004  /* Ok for [unqlite_open] */
#define UNQLITE_OPEN_EXCLUSIVE        0x00000008  /* VFS only */
#define UNQLITE_OPEN_TEMP_DB          0x00000010  /* VFS only */
#define UNQLITE_OPEN_NOMUTEX          0x00000020  /* Ok for [unqlite_open] */
#define UNQLITE_OPEN_OMIT_JOURNALING  0x00000040  /* Omit journaling for this database. Ok for [unqlite_open] */
#define UNQLITE_OPEN_IN_MEMORY        0x00000080  /* An in memory database. Ok for [unqlite_open]*/
#define UNQLITE_OPEN_MMAP             0x00000100  /* Obtain a memory view of the whole file. Ok for [unqlite_open] */
/*
 * Synchronization Type Flags
 *
 * When UnQLite invokes the xSync() method of an [unqlite_io_methods] object it uses
 * a combination of these integer values as the second argument.
 *
 * When the UNQLITE_SYNC_DATAONLY flag is used, it means that the sync operation only
 * needs to flush data to mass storage.  Inode information need not be flushed.
 * If the lower four bits of the flag equal UNQLITE_SYNC_NORMAL, that means to use normal
 * fsync() semantics. If the lower four bits equal UNQLITE_SYNC_FULL, that means to use
 * Mac OS X style fullsync instead of fsync().
 */
#define UNQLITE_SYNC_NORMAL        0x00002
#define UNQLITE_SYNC_FULL          0x00003
#define UNQLITE_SYNC_DATAONLY      0x00010
/*
 * File Locking Levels
 *
 * UnQLite uses one of these integer values as the second
 * argument to calls it makes to the xLock() and xUnlock() methods
 * of an [unqlite_io_methods] object.
 */
#define UNQLITE_LOCK_NONE          0
#define UNQLITE_LOCK_SHARED        1
#define UNQLITE_LOCK_RESERVED      2
#define UNQLITE_LOCK_PENDING       3
#define UNQLITE_LOCK_EXCLUSIVE     4
/*
 * CAPIREF: OS Interface: Open File Handle
 *
 * An [unqlite_file] object represents an open file in the [unqlite_vfs] OS interface
 * layer.
 * Individual OS interface implementations will want to subclass this object by appending
 * additional fields for their own use. The pMethods entry is a pointer to an
 * [unqlite_io_methods] object that defines methods for performing
 * I/O operations on the open file.
*/
typedef struct unqlite_file unqlite_file;
struct unqlite_file {
  const unqlite_io_methods *pMethods;  /* Methods for an open file. MUST BE FIRST */
};
/*
 * CAPIREF: OS Interface: File Methods Object
 *
 * Every file opened by the [unqlite_vfs] xOpen method populates an
 * [unqlite_file] object (or, more commonly, a subclass of the
 * [unqlite_file] object) with a pointer to an instance of this object.
 * This object defines the methods used to perform various operations
 * against the open file represented by the [unqlite_file] object.
 *
 * If the xOpen method sets the unqlite_file.pMethods element 
 * to a non-NULL pointer, then the unqlite_io_methods.xClose method
 * may be invoked even if the xOpen reported that it failed.  The
 * only way to prevent a call to xClose following a failed xOpen
 * is for the xOpen to set the unqlite_file.pMethods element to NULL.
 *
 * The flags argument to xSync may be one of [UNQLITE_SYNC_NORMAL] or
 * [UNQLITE_SYNC_FULL]. The first choice is the normal fsync().
 * The second choice is a Mac OS X style fullsync. The [UNQLITE_SYNC_DATAONLY]
 * flag may be ORed in to indicate that only the data of the file
 * and not its inode needs to be synced.
 *
 * The integer values to xLock() and xUnlock() are one of
 *
 * UNQLITE_LOCK_NONE
 * UNQLITE_LOCK_SHARED
 * UNQLITE_LOCK_RESERVED
 * UNQLITE_LOCK_PENDING
 * UNQLITE_LOCK_EXCLUSIVE
 * 
 * xLock() increases the lock. xUnlock() decreases the lock.
 * The xCheckReservedLock() method checks whether any database connection,
 * either in this process or in some other process, is holding a RESERVED,
 * PENDING, or EXCLUSIVE lock on the file. It returns true if such a lock exists
 * and false otherwise.
 * 
 * The xSectorSize() method returns the sector size of the device that underlies
 * the file. The sector size is the minimum write that can be performed without
 * disturbing other bytes in the file.
 *
 */
struct unqlite_io_methods {
  int iVersion;                 /* Structure version number (currently 1) */
  int (*xClose)(unqlite_file*);
  int (*xRead)(unqlite_file*, void*, unqlite_int64 iAmt, unqlite_int64 iOfst);
  int (*xWrite)(unqlite_file*, const void*, unqlite_int64 iAmt, unqlite_int64 iOfst);
  int (*xTruncate)(unqlite_file*, unqlite_int64 size);
  int (*xSync)(unqlite_file*, int flags);
  int (*xFileSize)(unqlite_file*, unqlite_int64 *pSize);
  int (*xLock)(unqlite_file*, int);
  int (*xUnlock)(unqlite_file*, int);
  int (*xCheckReservedLock)(unqlite_file*, int *pResOut);
  int (*xSectorSize)(unqlite_file*);
};
/*
 * CAPIREF: OS Interface Object
 *
 * An instance of the unqlite_vfs object defines the interface between
 * the UnQLite core and the underlying operating system.  The "vfs"
 * in the name of the object stands for "Virtual File System".
 *
 * Only a single vfs can be registered within the UnQLite core.
 * Vfs registration is done using the [unqlite_lib_config()] interface
 * with a configuration verb set to UNQLITE_LIB_CONFIG_VFS.
 * Note that Windows and UNIX (Linux, FreeBSD, Solaris, Mac OS X, etc.) users
 * does not have to worry about registering and installing a vfs since UnQLite
 * come with a built-in vfs for these platforms that implements most the methods
 * defined below.
 *
 * Clients running on exotic systems (ie: Other than Windows and UNIX systems)
 * must register their own vfs in order to be able to use the UnQLite library.
 *
 * The value of the iVersion field is initially 1 but may be larger in
 * future versions of UnQLite. 
 *
 * The szOsFile field is the size of the subclassed [unqlite_file] structure
 * used by this VFS. mxPathname is the maximum length of a pathname in this VFS.
 * 
 * At least szOsFile bytes of memory are allocated by UnQLite to hold the [unqlite_file]
 * structure passed as the third argument to xOpen. The xOpen method does not have to
 * allocate the structure; it should just fill it in. Note that the xOpen method must
 * set the unqlite_file.pMethods to either a valid [unqlite_io_methods] object or to NULL.
 * xOpen must do this even if the open fails. UnQLite expects that the unqlite_file.pMethods
 * element will be valid after xOpen returns regardless of the success or failure of the
 * xOpen call.
 *
 */
struct unqlite_vfs {
  const char *zName;       /* Name of this virtual file system [i.e: Windows, UNIX, etc.] */
  int iVersion;            /* Structure version number (currently 1) */
  int szOsFile;            /* Size of subclassed unqlite_file */
  int mxPathname;          /* Maximum file pathname length */
  int (*xOpen)(unqlite_vfs*, const char *zName, unqlite_file*,unsigned int flags);
  int (*xDelete)(unqlite_vfs*, const char *zName, int syncDir);
  int (*xAccess)(unqlite_vfs*, const char *zName, int flags, int *pResOut);
  int (*xFullPathname)(unqlite_vfs*, const char *zName,int buf_len,char *zBuf);
  int (*xTmpDir)(unqlite_vfs*,char *zBuf,int buf_len);
  int (*xSleep)(unqlite_vfs*, int microseconds);
  int (*xCurrentTime)(unqlite_vfs*,Sytm *pOut);
  int (*xGetLastError)(unqlite_vfs*, int, char *);
};
/*
 * Flags for the xAccess VFS method
 *
 * These integer constants can be used as the third parameter to
 * the xAccess method of an [unqlite_vfs] object.  They determine
 * what kind of permissions the xAccess method is looking for.
 * With UNQLITE_ACCESS_EXISTS, the xAccess method
 * simply checks whether the file exists.
 * With UNQLITE_ACCESS_READWRITE, the xAccess method
 * checks whether the named directory is both readable and writable
 * (in other words, if files can be added, removed, and renamed within
 * the directory).
 * The UNQLITE_ACCESS_READWRITE constant is currently used only by the
 * [temp_store_directory pragma], though this could change in a future
 * release of UnQLite.
 * With UNQLITE_ACCESS_READ, the xAccess method
 * checks whether the file is readable.  The UNQLITE_ACCESS_READ constant is
 * currently unused, though it might be used in a future release of
 * UnQLite.
 */
#define UNQLITE_ACCESS_EXISTS    0
#define UNQLITE_ACCESS_READWRITE 1   
#define UNQLITE_ACCESS_READ      2 
/*
 * The type used to represent a page number.  The first page in a file
 * is called page 1.  0 is used to represent "not a page".
 * A page number is an unsigned 64-bit integer.
 */
typedef sxu64 pgno;
/*
 * A database disk page is represented by an instance
 * of the follwoing structure.
 */
typedef struct unqlite_page unqlite_page;
struct unqlite_page
{
  unsigned char *zData;       /* Content of this page */
  void *pUserData;            /* Extra content */
  pgno pgno;                  /* Page number for this page */
};
/*
 * UnQLite handle to the underlying Key/Value Storage Engine (See below).
 */
typedef void * unqlite_kv_handle;
/*
 * UnQLite pager IO methods.
 *
 * An instance of the following structure define the exported methods of the UnQLite pager
 * to the underlying Key/Value storage engine.
 */
typedef struct unqlite_kv_io unqlite_kv_io;
struct unqlite_kv_io
{
	unqlite_kv_handle  pHandle;     /* UnQLite handle passed as the first parameter to the
									 * method defined below.
									 */
	unqlite_kv_methods *pMethods;   /* Underlying storage engine */
	/* Pager methods */
	int (*xGet)(unqlite_kv_handle,pgno,unqlite_page **);
	int (*xLookup)(unqlite_kv_handle,pgno,unqlite_page **);
	int (*xNew)(unqlite_kv_handle,unqlite_page **);
	int (*xWrite)(unqlite_page *);
	int (*xDontWrite)(unqlite_page *);
	int (*xDontJournal)(unqlite_page *);
	int (*xDontMkHot)(unqlite_page *);
	int (*xPageRef)(unqlite_page *);
	int (*xPageUnref)(unqlite_page *);
	int (*xPageSize)(unqlite_kv_handle);
	int (*xReadOnly)(unqlite_kv_handle);
	unsigned char * (*xTmpPage)(unqlite_kv_handle);
	void (*xSetUnpin)(unqlite_kv_handle,void (*xPageUnpin)(void *)); 
	void (*xSetReload)(unqlite_kv_handle,void (*xPageReload)(void *));
	void (*xErr)(unqlite_kv_handle,const char *);
};
/*
 * Key/Value Storage Engine Cursor Object
 *
 * An instance of a subclass of the following object defines a cursor
 * used to scan through a key-value storage engine.
 */
typedef struct unqlite_kv_cursor unqlite_kv_cursor;
struct unqlite_kv_cursor
{
  unqlite_kv_engine *pStore; /* Must be first */
  /* Subclasses will typically add additional fields */
};
/*
 * Possible seek positions.
 */
#define UNQLITE_CURSOR_MATCH_EXACT  1
#define UNQLITE_CURSOR_MATCH_LE     2
#define UNQLITE_CURSOR_MATCH_GE     3
/*
 * Key/Value Storage Engine.
 *
 * A Key-Value storage engine is defined by an instance of the following
 * object.
 * UnQLite works with run-time interchangeable storage engines (i.e. Hash, B+Tree, R+Tree, LSM, etc.).
 * The storage engine works with key/value pairs where both the key
 * and the value are byte arrays of arbitrary length and with no restrictions on content.
 * UnQLite come with two built-in KV storage engine: A Virtual Linear Hash (VLH) storage
 * engine is used for persistent on-disk databases with O(1) lookup time and an in-memory
 * hash-table or Red-black tree storage engine is used for in-memory databases.
 * Future versions of UnQLite might add other built-in storage engines (i.e. LSM). 
 * Registration of a Key/Value storage engine at run-time is done via [unqlite_lib_config()]
 * with a configuration verb set to UNQLITE_LIB_CONFIG_STORAGE_ENGINE.
 */
struct unqlite_kv_engine
{
  const unqlite_kv_io *pIo; /* IO methods: MUST be first */
   /* Subclasses will typically add additional fields */
};
/*
 * Key/Value Storage Engine Virtual Method Table.
 *
 * Key/Value storage engine methods is defined by an instance of the following
 * object.
 * Registration of a Key/Value storage engine at run-time is done via [unqlite_lib_config()]
 * with a configuration verb set to UNQLITE_LIB_CONFIG_STORAGE_ENGINE.
 */
struct unqlite_kv_methods
{
  const char *zName; /* Storage engine name [i.e. Hash, B+tree, LSM, R-tree, Mem, etc.]*/
  int szKv;          /* 'unqlite_kv_engine' subclass size */
  int szCursor;      /* 'unqlite_kv_cursor' subclass size */
  int iVersion;      /* Structure version, currently 1 */
  /* Storage engine methods */
  int (*xInit)(unqlite_kv_engine *,int iPageSize);
  void (*xRelease)(unqlite_kv_engine *);
  int (*xConfig)(unqlite_kv_engine *,int op,va_list ap);
  int (*xOpen)(unqlite_kv_engine *,pgno);
  int (*xReplace)(
	  unqlite_kv_engine *,
	  const void *pKey,int nKeyLen,
	  const void *pData,unqlite_int64 nDataLen
	  ); 
    int (*xAppend)(
	  unqlite_kv_engine *,
	  const void *pKey,int nKeyLen,
	  const void *pData,unqlite_int64 nDataLen
	  );
  void (*xCursorInit)(unqlite_kv_cursor *);
  int (*xSeek)(unqlite_kv_cursor *,const void *pKey,int nByte,int iPos); /* Mandatory */
  int (*xFirst)(unqlite_kv_cursor *);
  int (*xLast)(unqlite_kv_cursor *);
  int (*xValid)(unqlite_kv_cursor *);
  int (*xNext)(unqlite_kv_cursor *);
  int (*xPrev)(unqlite_kv_cursor *);
  int (*xDelete)(unqlite_kv_cursor *);
  int (*xKeyLength)(unqlite_kv_cursor *,int *);
  int (*xKey)(unqlite_kv_cursor *,int (*xConsumer)(const void *,unsigned int,void *),void *pUserData);
  int (*xDataLength)(unqlite_kv_cursor *,unqlite_int64 *);
  int (*xData)(unqlite_kv_cursor *,int (*xConsumer)(const void *,unsigned int,void *),void *pUserData);
  void (*xReset)(unqlite_kv_cursor *);
  void (*xCursorRelease)(unqlite_kv_cursor *);
};
/*
 * UnQLite journal file suffix.
 */
#ifndef UNQLITE_JOURNAL_FILE_SUFFIX
#define UNQLITE_JOURNAL_FILE_SUFFIX "_unqlite_journal"
#endif
/*
 * Call Context - Error Message Serverity Level.
 *
 * The following constans are the allowed severity level that can
 * passed as the second argument to the [unqlite_context_throw_error()] or
 * [unqlite_context_throw_error_format()] interfaces.
 * Refer to the official documentation for additional information.
 */
#define UNQLITE_CTX_ERR       1 /* Call context error such as unexpected number of arguments, invalid types and so on. */
#define UNQLITE_CTX_WARNING   2 /* Call context Warning */
#define UNQLITE_CTX_NOTICE    3 /* Call context Notice */
/* 
 * C-API-REF: Please refer to the official documentation for interfaces
 * purpose and expected parameters. 
 */ 

/* Database Engine Handle */
UNQLITE_APIEXPORT int unqlite_open(unqlite **ppDB,const char *zFilename,unsigned int iMode);
UNQLITE_APIEXPORT int unqlite_config(unqlite *pDb,int nOp,...);
UNQLITE_APIEXPORT int unqlite_close(unqlite *pDb);

/* Key/Value (KV) Store Interfaces */
UNQLITE_APIEXPORT int unqlite_kv_store(unqlite *pDb,const void *pKey,int nKeyLen,const void *pData,unqlite_int64 nDataLen);
UNQLITE_APIEXPORT int unqlite_kv_append(unqlite *pDb,const void *pKey,int nKeyLen,const void *pData,unqlite_int64 nDataLen);
UNQLITE_APIEXPORT int unqlite_kv_store_fmt(unqlite *pDb,const void *pKey,int nKeyLen,const char *zFormat,...);
UNQLITE_APIEXPORT int unqlite_kv_append_fmt(unqlite *pDb,const void *pKey,int nKeyLen,const char *zFormat,...);
UNQLITE_APIEXPORT int unqlite_kv_fetch(unqlite *pDb,const void *pKey,int nKeyLen,void *pBuf,unqlite_int64 /* in|out */*pBufLen);
UNQLITE_APIEXPORT int unqlite_kv_fetch_callback(unqlite *pDb,const void *pKey,
	                    int nKeyLen,int (*xConsumer)(const void *,unsigned int,void *),void *pUserData);
UNQLITE_APIEXPORT int unqlite_kv_delete(unqlite *pDb,const void *pKey,int nKeyLen);
UNQLITE_APIEXPORT int unqlite_kv_config(unqlite *pDb,int iOp,...);

/* Document (JSON) Store Interfaces powered by the Jx9 Scripting Language */
UNQLITE_APIEXPORT int unqlite_compile(unqlite *pDb,const char *zJx9,int nByte,unqlite_vm **ppOut);
UNQLITE_APIEXPORT int unqlite_compile_file(unqlite *pDb,const char *zPath,unqlite_vm **ppOut);
UNQLITE_APIEXPORT int unqlite_vm_config(unqlite_vm *pVm,int iOp,...);
UNQLITE_APIEXPORT int unqlite_vm_exec(unqlite_vm *pVm);
UNQLITE_APIEXPORT int unqlite_vm_reset(unqlite_vm *pVm);
UNQLITE_APIEXPORT int unqlite_vm_release(unqlite_vm *pVm);
UNQLITE_APIEXPORT int unqlite_vm_dump(unqlite_vm *pVm, int (*xConsumer)(const void *, unsigned int, void *), void *pUserData);
UNQLITE_APIEXPORT unqlite_value * unqlite_vm_extract_variable(unqlite_vm *pVm,const char *zVarname);

/*  Cursor Iterator Interfaces */
UNQLITE_APIEXPORT int unqlite_kv_cursor_init(unqlite *pDb,unqlite_kv_cursor **ppOut);
UNQLITE_APIEXPORT int unqlite_kv_cursor_release(unqlite *pDb,unqlite_kv_cursor *pCur);
UNQLITE_APIEXPORT int unqlite_kv_cursor_seek(unqlite_kv_cursor *pCursor,const void *pKey,int nKeyLen,int iPos);
UNQLITE_APIEXPORT int unqlite_kv_cursor_first_entry(unqlite_kv_cursor *pCursor);
UNQLITE_APIEXPORT int unqlite_kv_cursor_last_entry(unqlite_kv_cursor *pCursor);
UNQLITE_APIEXPORT int unqlite_kv_cursor_valid_entry(unqlite_kv_cursor *pCursor);
UNQLITE_APIEXPORT int unqlite_kv_cursor_next_entry(unqlite_kv_cursor *pCursor);
UNQLITE_APIEXPORT int unqlite_kv_cursor_prev_entry(unqlite_kv_cursor *pCursor);
UNQLITE_APIEXPORT int unqlite_kv_cursor_key(unqlite_kv_cursor *pCursor,void *pBuf,int *pnByte);
UNQLITE_APIEXPORT int unqlite_kv_cursor_key_callback(unqlite_kv_cursor *pCursor,int (*xConsumer)(const void *,unsigned int,void *),void *pUserData);
UNQLITE_APIEXPORT int unqlite_kv_cursor_data(unqlite_kv_cursor *pCursor,void *pBuf,unqlite_int64 *pnData);
UNQLITE_APIEXPORT int unqlite_kv_cursor_data_callback(unqlite_kv_cursor *pCursor,int (*xConsumer)(const void *,unsigned int,void *),void *pUserData);
UNQLITE_APIEXPORT int unqlite_kv_cursor_delete_entry(unqlite_kv_cursor *pCursor);
UNQLITE_APIEXPORT int unqlite_kv_cursor_reset(unqlite_kv_cursor *pCursor);

/* Manual Transaction Manager */
UNQLITE_APIEXPORT int unqlite_begin(unqlite *pDb);
UNQLITE_APIEXPORT int unqlite_commit(unqlite *pDb);
UNQLITE_APIEXPORT int unqlite_rollback(unqlite *pDb);

/* Utility interfaces */
UNQLITE_APIEXPORT int unqlite_util_load_mmaped_file(const char *zFile,void **ppMap,unqlite_int64 *pFileSize);
UNQLITE_APIEXPORT int unqlite_util_release_mmaped_file(void *pMap,unqlite_int64 iFileSize);
UNQLITE_APIEXPORT int unqlite_util_random_string(unqlite *pDb,char *zBuf,unsigned int buf_size);
UNQLITE_APIEXPORT unsigned int unqlite_util_random_num(unqlite *pDb);

/* In-process extending interfaces */
UNQLITE_APIEXPORT int unqlite_create_function(unqlite_vm *pVm,const char *zName,int (*xFunc)(unqlite_context *,int,unqlite_value **),void *pUserData);
UNQLITE_APIEXPORT int unqlite_delete_function(unqlite_vm *pVm, const char *zName);
UNQLITE_APIEXPORT int unqlite_create_constant(unqlite_vm *pVm,const char *zName,void (*xExpand)(unqlite_value *, void *),void *pUserData);
UNQLITE_APIEXPORT int unqlite_delete_constant(unqlite_vm *pVm, const char *zName);

/* On Demand Object allocation interfaces */
UNQLITE_APIEXPORT unqlite_value * unqlite_vm_new_scalar(unqlite_vm *pVm);
UNQLITE_APIEXPORT unqlite_value * unqlite_vm_new_array(unqlite_vm *pVm);
UNQLITE_APIEXPORT int unqlite_vm_release_value(unqlite_vm *pVm,unqlite_value *pValue);
UNQLITE_APIEXPORT unqlite_value * unqlite_context_new_scalar(unqlite_context *pCtx);
UNQLITE_APIEXPORT unqlite_value * unqlite_context_new_array(unqlite_context *pCtx);
UNQLITE_APIEXPORT void unqlite_context_release_value(unqlite_context *pCtx,unqlite_value *pValue);

/* Dynamically Typed Value Object Management Interfaces */
UNQLITE_APIEXPORT int unqlite_value_int(unqlite_value *pVal, int iValue);
UNQLITE_APIEXPORT int unqlite_value_int64(unqlite_value *pVal, unqlite_int64 iValue);
UNQLITE_APIEXPORT int unqlite_value_bool(unqlite_value *pVal, int iBool);
UNQLITE_APIEXPORT int unqlite_value_null(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_double(unqlite_value *pVal, double Value);
UNQLITE_APIEXPORT int unqlite_value_string(unqlite_value *pVal, const char *zString, int nLen);
UNQLITE_APIEXPORT int unqlite_value_string_format(unqlite_value *pVal, const char *zFormat,...);
UNQLITE_APIEXPORT int unqlite_value_reset_string_cursor(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_resource(unqlite_value *pVal, void *pUserData);
UNQLITE_APIEXPORT int unqlite_value_release(unqlite_value *pVal);

/* Foreign Function Parameter Values */
UNQLITE_APIEXPORT int unqlite_value_to_int(unqlite_value *pValue);
UNQLITE_APIEXPORT int unqlite_value_to_bool(unqlite_value *pValue);
UNQLITE_APIEXPORT unqlite_int64 unqlite_value_to_int64(unqlite_value *pValue);
UNQLITE_APIEXPORT double unqlite_value_to_double(unqlite_value *pValue);
UNQLITE_APIEXPORT const char * unqlite_value_to_string(unqlite_value *pValue, int *pLen);
UNQLITE_APIEXPORT void * unqlite_value_to_resource(unqlite_value *pValue);
UNQLITE_APIEXPORT int unqlite_value_compare(unqlite_value *pLeft, unqlite_value *pRight, int bStrict);

/* Setting The Result Of A Foreign Function */
UNQLITE_APIEXPORT int unqlite_result_int(unqlite_context *pCtx, int iValue);
UNQLITE_APIEXPORT int unqlite_result_int64(unqlite_context *pCtx, unqlite_int64 iValue);
UNQLITE_APIEXPORT int unqlite_result_bool(unqlite_context *pCtx, int iBool);
UNQLITE_APIEXPORT int unqlite_result_double(unqlite_context *pCtx, double Value);
UNQLITE_APIEXPORT int unqlite_result_null(unqlite_context *pCtx);
UNQLITE_APIEXPORT int unqlite_result_string(unqlite_context *pCtx, const char *zString, int nLen);
UNQLITE_APIEXPORT int unqlite_result_string_format(unqlite_context *pCtx, const char *zFormat, ...);
UNQLITE_APIEXPORT int unqlite_result_value(unqlite_context *pCtx, unqlite_value *pValue);
UNQLITE_APIEXPORT int unqlite_result_resource(unqlite_context *pCtx, void *pUserData);

/* Dynamically Typed Value Object Query Interfaces */
UNQLITE_APIEXPORT int unqlite_value_is_int(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_float(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_bool(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_string(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_null(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_numeric(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_callable(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_scalar(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_json_array(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_json_object(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_resource(unqlite_value *pVal);
UNQLITE_APIEXPORT int unqlite_value_is_empty(unqlite_value *pVal);

/* JSON Array/Object Management Interfaces */
UNQLITE_APIEXPORT unqlite_value * unqlite_array_fetch(unqlite_value *pArray, const char *zKey, int nByte);
UNQLITE_APIEXPORT int unqlite_array_walk(unqlite_value *pArray, int (*xWalk)(unqlite_value *, unqlite_value *, void *), void *pUserData);
UNQLITE_APIEXPORT int unqlite_array_add_elem(unqlite_value *pArray, unqlite_value *pKey, unqlite_value *pValue);
UNQLITE_APIEXPORT int unqlite_array_add_strkey_elem(unqlite_value *pArray, const char *zKey, unqlite_value *pValue);
UNQLITE_APIEXPORT int unqlite_array_count(unqlite_value *pArray);

/* Call Context Handling Interfaces */
UNQLITE_APIEXPORT int unqlite_context_output(unqlite_context *pCtx, const char *zString, int nLen);
UNQLITE_APIEXPORT int unqlite_context_output_format(unqlite_context *pCtx,const char *zFormat, ...);
UNQLITE_APIEXPORT int unqlite_context_throw_error(unqlite_context *pCtx, int iErr, const char *zErr);
UNQLITE_APIEXPORT int unqlite_context_throw_error_format(unqlite_context *pCtx, int iErr, const char *zFormat, ...);
UNQLITE_APIEXPORT unsigned int unqlite_context_random_num(unqlite_context *pCtx);
UNQLITE_APIEXPORT int unqlite_context_random_string(unqlite_context *pCtx, char *zBuf, int nBuflen);
UNQLITE_APIEXPORT void * unqlite_context_user_data(unqlite_context *pCtx);
UNQLITE_APIEXPORT int unqlite_context_push_aux_data(unqlite_context *pCtx, void *pUserData);
UNQLITE_APIEXPORT void * unqlite_context_peek_aux_data(unqlite_context *pCtx);
UNQLITE_APIEXPORT unsigned int unqlite_context_result_buf_length(unqlite_context *pCtx);
UNQLITE_APIEXPORT const char * unqlite_function_name(unqlite_context *pCtx);

/* Call Context Memory Management Interfaces */
UNQLITE_APIEXPORT void * unqlite_context_alloc_chunk(unqlite_context *pCtx,unsigned int nByte,int ZeroChunk,int AutoRelease);
UNQLITE_APIEXPORT void * unqlite_context_realloc_chunk(unqlite_context *pCtx,void *pChunk,unsigned int nByte);
UNQLITE_APIEXPORT void unqlite_context_free_chunk(unqlite_context *pCtx,void *pChunk);

/* Global Library Management Interfaces */
UNQLITE_APIEXPORT int unqlite_lib_config(int nConfigOp,...);
UNQLITE_APIEXPORT int unqlite_lib_init(void);
UNQLITE_APIEXPORT int unqlite_lib_shutdown(void);
UNQLITE_APIEXPORT int unqlite_lib_is_threadsafe(void);
UNQLITE_APIEXPORT const char * unqlite_lib_version(void);
UNQLITE_APIEXPORT const char * unqlite_lib_signature(void);
UNQLITE_APIEXPORT const char * unqlite_lib_ident(void);
UNQLITE_APIEXPORT const char * unqlite_lib_copyright(void);

#endif /* _UNQLITE_H_ */