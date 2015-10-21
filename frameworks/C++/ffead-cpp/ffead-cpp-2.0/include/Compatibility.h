
#ifndef MINGW_MISSING_H_
#define MINGW_MISSING_H_
#include "AppDefines.h"
#include "iostream"
#include <errno.h>
#include <signal.h>
#include "string"
#include "cstring"
#include <stdio.h>
#include <stdlib.h>
#include "vector"
#include "sstream"
#include <fcntl.h>
#include <time.h>
#include <sys/time.h>
#include <sys/types.h>
#include <time.h>
#include <iostream>


#if defined(OS_MINGW)
#undef _WIN32_WINNT
#define _WIN32_WINNT 0x0600
#define HAVE_STRUCT_TIMESPEC 1
#include <unistd.h>
#include <pthread.h>
#define USE_MINGW_SELECT 1
#define _WINDOWS_ 1
#define SQL_WCHART_CONVERT 1
#include "windef.h"
#include "WinBase.h"
#include <ws2tcpip.h>
#include <winsock2.h>
#include <Mswsock.h>


#ifndef OVERLAPPED_ENTRY /* This is a very ugly hack, but mingw doesn't have
                            these defines. */

WINBASEAPI BOOL WINAPI
GetQueuedCompletionStatusEx(HANDLE CompletionPort,
                            LPOVERLAPPED_ENTRY lpCompletionPortEntries,
                            ULONG ulCount,
                            PULONG ulNumEntriesRemoved,
                            DWORD dwMilliseconds,
                            BOOL fAlertable);
#endif

#ifndef SOCKLEN_T_DEFINED
#define SOCKLEN_T_DEFINED 1
typedef int socklen_t;
#endif

#ifndef NS_INADDRSZ
#define NS_INADDRSZ	4
#endif
#ifndef NS_IN6ADDRSZ
#define NS_IN6ADDRSZ	16
#endif
#ifndef NS_INT16SZ
#define NS_INT16SZ	2
#endif

#ifndef INET6_ADDRSTRLEN
# define INET6_ADDRSTRLEN       46
#endif /* INET6_ADDRSTRLEN */

#define hstrerror strerror

#define S_IFLNK    0120000 /* Symbolic link */
#define S_ISLNK(x) (((x) & S_IFMT) == S_IFLNK)
#define S_ISSOCK(x) 0
#define S_IRGRP 0
#define S_IWGRP 0
#define S_IXGRP 0
#define S_ISGID 0
#define S_IROTH 0
#define S_IXOTH 0

#define WIFEXITED(x) 1
#define WIFSIGNALED(x) 0
#define WEXITSTATUS(x) ((x) & 0xff)
#define WTERMSIG(x) SIGTERM

#define SIGHUP 1
#define SIGQUIT 3
#define SIGKILL 9
#define SIGPIPE 13
#define SIGALRM 14
#define SIGCHLD 17

#define F_GETFD 1
#define F_SETFD 2
#define FD_CLOEXEC 0x1

#ifndef CLOCK_REALTIME
#define CLOCK_REALTIME  0
#endif

#ifndef CLOCK_MONOTONIC
#define CLOCK_MONOTONIC 1  
#endif

#ifndef HAVE_DECL_NANOSLEEP
#define HAVE_DECL_NANOSLEEP 0
#endif


/* total seconds since epoch until start of unix time (january 1, 1970 00:00 UTC) */
#define _DOVA_UNIX_SECONDS 11644473600

#ifndef OS_MINGW_W64
typedef int pid_t;
struct tm *gmtime_r(const time_t *timep, struct tm *result);
struct tm *localtime_r(const time_t *timep, struct tm *result);
#endif

int clock_gettime (int clockid, struct timespec *tp);
const char *inet_ntop(int af, const void *src, char *dst, socklen_t size);
int inet_pton(int af, const char *src, void *dst);
int inet_pton4(const char *src, unsigned char *dst);
int inet_pton6(const char *src, unsigned char *dst);
typedef DWORD NUMEVENTS;


#else

#if defined(OS_DARWIN)
#include <mach/clock.h>
#include <mach/mach.h>

#ifndef CLOCK_REALTIME
#define CLOCK_REALTIME  0
#endif

#ifndef CLOCK_MONOTONIC
#define CLOCK_MONOTONIC 1
#endif

static inline int clock_gettime (int clockid, struct timespec *ts)
{
	clock_serv_t cclock;
	mach_timespec_t mts;
	host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
	clock_get_time(cclock, &mts);
	mach_port_deallocate(mach_task_self(), cclock);
	ts->tv_sec = mts.tv_sec;
	ts->tv_nsec = mts.tv_nsec;
	return 0;
}
#endif



typedef int SOCKET;
typedef int NUMEVENTS;
#define INVALID_SOCKET -1  // WinSock invalid socket
#define SOCKET_ERROR   -1  // basic WinSock error
#define closesocket(s) close(s);  // Unix uses file descriptors, WinSock doesn't...

#include <netinet/in.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <arpa/inet.h>
#include <sys/wait.h>
#include <sys/ioctl.h>
#include <sys/resource.h>
#include <unistd.h>
#include <pthread.h>
#endif
#endif /* MINGW_MISSING_H_ */
