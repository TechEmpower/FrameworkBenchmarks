/*
	Copyright 2009-2013, Sumeet Chhetri

    Licensed under the Apache License, Version 2.0 (const the& "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
/*
 * SelEpolKqEvPrt.h
 *
 *  Created on: 30-Dec-2012
 *      Author: sumeetc
 */

#ifndef SELEPOLKQEVPRT_H_
#define SELEPOLKQEVPRT_H_
#include "Compatibility.h"
#include "map"
#include "Mutex.h"
using namespace std;
#define MAXDESCRIPTORS 1024
#define OP_READ     0
#define OP_WRITE    1

#if USE_EVPORT == 1
	#undef USE_EPOLL
	#undef USE_KQUEUE
	#undef USE_DEVPOLL
	#undef USE_POLL
	#undef USE_SELECT
	#undef USE_WIN_IOCP
#include <port.h>
#include <poll.h>
#elif USE_EPOLL == 1
	#undef USE_EVPORT
	#undef USE_KQUEUE
	#undef USE_DEVPOLL
	#undef USE_POLL
	#undef USE_SELECT
	#undef USE_WIN_IOCP
#undef USE_WIN_IOCP
#include <sys/epoll.h>
#elif USE_KQUEUE == 1
	#undef USE_EVPORT
	#undef USE_EPOLL
	#undef USE_DEVPOLL
	#undef USE_POLL
	#undef USE_SELECT
	#undef USE_WIN_IOCP
#include <sys/event.h>
#elif USE_DEVPOLL == 1
	#undef USE_EPOLL
	#undef USE_KQUEUE
	#undef USE_EVPORT
	#undef USE_POLL
	#undef USE_SELECT
	#undef USE_WIN_IOCP
#include <sys/devpoll.h>
#elif USE_POLL == 1 && !defined(OS_CYGWIN)
	#undef USE_EPOLL
	#undef USE_KQUEUE
	#undef USE_EVPORT
	#undef USE_DEVPOLL
	#undef USE_SELECT
	#undef USE_WIN_IOCP
#include <poll.h>
#elif USE_POLL == 1
	#define USE_SELECT 1
	#undef USE_EPOLL
	#undef USE_KQUEUE
	#undef USE_EVPORT
	#undef USE_DEVPOLL
	#undef USE_POLL
	#undef USE_WIN_IOCP
#include <sys/select.h>
#elif USE_WIN_IOCP == 1
	#undef USE_SELECT
	#undef USE_EPOLL
	#undef USE_KQUEUE
	#undef USE_EVPORT
	#undef USE_DEVPOLL
	#undef USE_POLL
	#undef USE_SELECT
	#define IOCPKEY_IO    56L
	class IOOperation {
		OVERLAPPED* o;
		SOCKET sock;
		friend class SelEpolKqEvPrt;
	};
	class SingleIOOperation {
		OVERLAPPED o;
		SOCKET sock;
		friend class SelEpolKqEvPrt;
		friend class IOOverlappedEntry;
	};
	class IOOverlappedEntry
	{
		ULONG_PTR key;
		SingleIOOperation* o;
		ULONG_PTR internal;
		DWORD  qty;
		friend class SelEpolKqEvPrt;
	};
#elif USE_MINGW_SELECT == 1
	#undef USE_EPOLL
	#undef USE_KQUEUE
	#undef USE_EVPORT
	#undef USE_DEVPOLL
	#undef USE_POLL
	#undef USE_SELECT
	#undef USE_WIN_IOCP
#elif USE_SELECT == 1
	#undef USE_EPOLL
	#undef USE_KQUEUE
	#undef USE_EVPORT
	#undef USE_DEVPOLL
	#undef USE_POLL
	#undef USE_WIN_IOCP
#include <sys/select.h>
#endif

class SelEpolKqEvPrt {
	bool listenerMode;
	int timeoutMilis;
	SOCKET sockfd;
	SOCKET curfds;
	Mutex l;
	#ifdef USE_WIN_IOCP
		map<SOCKET, void*> cntxtMap;
		vector<void*> psocks;
		HANDLE iocpPort;
		bool initIOCP();
		bool addToIOCP(void *p);
		void iocpRecv(const SOCKET& sock, const LPWSAOVERLAPPED& o);
	#elif USE_MINGW_SELECT
		SOCKET fdMax;
		int fdsetSize;
		fd_set readfds;  // temp file descriptor list for select()
		fd_set master;
	#elif USE_SELECT
		int fdMax, fdsetSize;
		fd_set readfds[MAXDESCRIPTORS/FD_SETSIZE];  // temp file descriptor list for select()
		fd_set master[MAXDESCRIPTORS/FD_SETSIZE];
	#elif defined USE_EPOLL
		struct epoll_event ev;
		struct epoll_event events[MAXDESCRIPTORS];
		int epoll_handle;
	#elif defined USE_KQUEUE
		int kq;
		struct kevent evlist[MAXDESCRIPTORS];
	    struct kevent change;
	#elif defined USE_DEVPOLL
	    int dev_poll_fd;
	    struct pollfd polled_fds[MAXDESCRIPTORS];
	#elif defined USE_EVPORT
	    int port;
	    port_event_t evlist[MAXDESCRIPTORS];
	#elif defined USE_POLL
	    nfds_t nfds;
	    struct pollfd *polled_fds;
	#endif
public:
	SelEpolKqEvPrt();
	virtual ~SelEpolKqEvPrt();
	void initialize(SOCKET sockfd, const int& timeout);
	void initialize(const int& timeout);
	int getEvents();
	SOCKET getDescriptor(const SOCKET& index);
	bool isListeningDescriptor(const SOCKET& descriptor);
	bool registerForEvent(const SOCKET& descriptor);
	bool unRegisterForEvent(const SOCKET& descriptor);
	void reRegisterServerSock();
	bool isInvalidDescriptor(const SOCKET& index);
	void lock();
	void unlock();
};

#endif /* SELEPOLKQEVPRT_H_ */
