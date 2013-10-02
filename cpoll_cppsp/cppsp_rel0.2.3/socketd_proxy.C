#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <sys/types.h>
#include <sys/socket.h>
#include <map>
#include <set>
#include <iostream>
#include <string>
#include <arpa/inet.h>
#include <unistd.h>
#include <dlfcn.h>
#include <socketd.H>
#include <cpoll/statemachines.H>
#include <cpoll/sendfd.H>
#include <fcntl.h>
#include <poll.h>


using namespace std;
using namespace socketd;
struct scopeLock
{
	pthread_mutex_t& mutex;
	scopeLock(pthread_mutex_t& m):mutex(m)
	{pthread_mutex_lock(&mutex);}
	~scopeLock()
	{pthread_mutex_unlock(&mutex);}
};
static void initMutex(pthread_mutex_t& mutex) {
	pthread_mutexattr_t attr;
	pthread_mutexattr_init(&attr);
	pthread_mutexattr_settype(&attr, PTHREAD_MUTEX_RECURSIVE);
	int e;
	if((e=pthread_mutex_init(&mutex,&attr))!=0) {
		const char* err=strerror(e);
		throw runtime_error("mutex initialization failed: "+string(err));
	}
	pthread_mutexattr_destroy(&attr);
}

static pthread_mutex_t mutex;
struct vhostInfo
{
	string name;
	string authToken;
	int fd;
	bool attach;
};
struct config
{
	map<int, vhostInfo> mappings;
	vhostInfo defaultVhost;
	bool hasDefault;
};
static bool configLoaded=false;
static config cfg;
static set<int> fds;
struct bufferInfo
{
	uint8_t* buf;
	int offset;
	int len;
};
static map<int,bufferInfo> fdbuffers;
static void loadConfig() {
	if(configLoaded)return;
	configLoaded=true;
	char* tmp=getenv("SOCKETD_FD");
	if((cfg.hasDefault=(tmp!=NULL))) {
		cfg.defaultVhost.attach=false;
		cfg.defaultVhost.fd=atoi(tmp);
	}
	initMutex(mutex);
	//cfg.mappings.insert({12580,{"vhost1",""}});
}
typedef int (*bind_def)(int sockfd, const struct sockaddr *addr,
                socklen_t addrlen);
typedef int (*accept_def)(int sockfd, struct sockaddr *addr, socklen_t *addrlen);
typedef int (*accept4_def)(int sockfd, struct sockaddr *addr,
                   socklen_t *addrlen, int flags);
typedef int (*listen_def)(int sockfd, int backlog);
typedef ssize_t (*recv_def)(int sockfd, void *buf, size_t len, int flags);
typedef ssize_t (*read_def)(int fd, void *buf, size_t count);
typedef int (*shutdown_def)(int sockfd, int how);
typedef int (*close_def)(int fd);
typedef int (*dup2_def)(int oldfd, int newfd);
typedef int (*dup3_def)(int oldfd, int newfd, int flags);

static bind_def prev_bind=NULL;
static accept_def prev_accept=NULL;
static accept4_def prev_accept4=NULL;
static listen_def prev_listen=NULL;
static recv_def prev_recv=NULL;
static read_def prev_read=NULL;
static shutdown_def prev_shutdown=NULL;
static close_def prev_close=NULL;
static dup2_def prev_dup2=NULL;
static dup3_def prev_dup3=NULL;
static int getPort(const struct sockaddr *addr, socklen_t addrlen) {
	switch(addr->sa_family) {
		case AF_INET:
			return ntohs(((const sockaddr_in*)addr)->sin_port);
		default:
			return -1;
	}
}
static int fixBind(int sockfd, const vhostInfo& vh) {
	if(vh.attach) {
		
	} else {
		if(prev_dup2==NULL) {
			prev_dup2=(dup2_def)dlsym(RTLD_NEXT, "dup2");
		}
		prev_dup2(vh.fd,sockfd);
		fds.insert(sockfd);
	}
	return 0;
}
extern "C" int bind(int sockfd, const struct sockaddr *addr, socklen_t addrlen) {
	if(prev_bind==NULL) {
		prev_bind=(bind_def)dlsym(RTLD_NEXT, "bind");
	}
	printf("bind()\n");
	int p=getPort(addr,addrlen);
	if(p<0) goto aaa;
	{
		scopeLock l(mutex);
		loadConfig();
		auto it=cfg.mappings.find(p);
		if(it==cfg.mappings.end()) {
			if(cfg.hasDefault) return fixBind(sockfd,cfg.defaultVhost);
		} else fixBind(sockfd,(*it).second);
	}
aaa:
	return prev_bind(sockfd,addr,addrlen);
}
extern "C" int listen(int sockfd, int backlog) {
	if(prev_listen==NULL) {
		prev_listen=(listen_def)dlsym(RTLD_NEXT, "listen");
	}
	{
		scopeLock l(mutex);
		if(fds.count(sockfd)>0) return 0;
	}
	return prev_listen(sockfd, backlog);
}

static int fixAccept(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
	protocolHeader ph;
	prot_handleConnection ph1;
	int r;
	r=recv(sockfd,&ph,sizeof(ph),0);
	if(r<0) return -1;
	if(r==0) exit(0);
	if(r!=sizeof(ph)) {
		fprintf(stderr,"failed to read struct protocolHeader (%i bytes) from fd %i: got %i bytes\n"
				,sizeof(protocolHeader),r,sockfd);
		exit(0);
	}
	if(ph.type!=protocolHeader::handleConnection) {
		fprintf(stderr,"protocolHeader.type is not handleConnection; it is %i\n",(int)ph.type);
		exit(0);
	}
	if((r=recv(sockfd,&ph1,sizeof(ph1),MSG_WAITALL))!=sizeof(ph1)) {
		fprintf(stderr,"failed to read struct prot_handleConnection (%i bytes) from fd %i: got %i bytes\n"
				,sizeof(prot_handleConnection),r,sockfd);
		exit(0);
	}
aaaaa:
	int fd=recvfd(sockfd,MSG_WAITALL);
	if(fd<0) {
		if(errno==EAGAIN || errno==EWOULDBLOCK) {
			pollfd pfd;
			pfd.fd = sockfd;
			pfd.events = POLLIN;
			if(poll(&pfd, 1, -1)<=0) return -1;
			if(!(pfd.revents&POLLIN)) return -1;
			goto aaaaa;
		}
		fprintf(stderr,"failed to recvfd() from fd %i; errno: %s\n",sockfd,strerror(errno));
		exit(0);
	}
	if(ph1.bufferLen<=0) return fd;
	getpeername(fd, addr, addrlen);
	bufferInfo bi;
	bi.buf=(uint8_t*)malloc(ph1.bufferLen);
	if(bi.buf==NULL) {
		close(fd);
		errno=ENOMEM;
		return -1;
	}
	bi.offset=0;
	bi.len=ph1.bufferLen;
bbbbb:
	if((r=recv(sockfd,bi.buf,bi.len,MSG_WAITALL))!=bi.len) {
		if(r<0 && (errno==EAGAIN||errno==EWOULDBLOCK)) {
			pollfd pfd;
			pfd.fd = sockfd;
			pfd.events = POLLIN;
			if(poll(&pfd, 1, -1)<=0) return -1;
			if(!(pfd.revents&POLLIN)) return -1;
			goto bbbbb;
		}
		fprintf(stderr,"failed to read %i bytes from fd %i: got %i bytes; errno: %s\n",bi.len,sockfd,r,strerror(errno));
		exit(0);
	}
	fdbuffers.insert({fd,bi});
	
	ph.type=protocolHeader::ackConnection;
	prot_ackConnection ack;
	ack.id=ph1.id;
	ack.success=true;
	if(send(sockfd,&ph,sizeof(ph),MSG_DONTWAIT)==sizeof(ph)) {
		send(sockfd,&ack,sizeof(ack),MSG_DONTWAIT);
	}
	
	return fd;
}
static int tryFixAccept(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
	{
		scopeLock l(mutex);
		if(fds.count(sockfd)<=0) return -2;
	}
	return fixAccept(sockfd, addr, addrlen);
}
extern "C" int accept(int sockfd, struct sockaddr *addr, socklen_t *addrlen) {
	if(prev_accept==NULL) {
		prev_accept=(accept_def)dlsym(RTLD_NEXT, "accept");
	}
	int fd=tryFixAccept(sockfd,addr,addrlen);
	if(fd==-2) return prev_accept(sockfd, addr, addrlen);
	else {
		if(fd>=0) {
			int f = fcntl(fd, F_GETFL, 0);
			f=f&~O_NONBLOCK;
			f=f&~O_CLOEXEC;
			fcntl(fd, F_SETFL, f);
		}
		return fd;
	}
}

extern "C" int accept4(int sockfd, struct sockaddr *addr, socklen_t *addrlen, int flags) {
	if(prev_accept4==NULL) {
		prev_accept4=(accept4_def)dlsym(RTLD_NEXT, "accept4");
	}
	int fd=tryFixAccept(sockfd,addr,addrlen);
	if(fd==-2) return prev_accept4(sockfd, addr, addrlen, flags);
	else {
		if(fd>=0) {
			int f = fcntl(fd, F_GETFL, 0);
			if(flags&SOCK_NONBLOCK)
				f=f|O_NONBLOCK;
			else f=f&~O_NONBLOCK;
			if(flags&SOCK_CLOEXEC)
				f=f|O_CLOEXEC;
			else f=f&~O_CLOEXEC;
			fcntl(fd, F_SETFL, f);
		}
		return fd;
	}
}
#define __min(x,y) ((x<y)?x:y)
static inline bufferInfo* getBufferInfo(int sockfd) {
	scopeLock l(mutex);
	auto it=fdbuffers.find(sockfd);
	if(it!=fdbuffers.end()) {
		return &(*it).second;
	} else return NULL;
}
static int fixRead(bufferInfo* bi, int sockfd, void *buf, size_t len) {
	//assumes that the application does not call recv() or read() on the same socket
	//from 2 threads at once
	int minLen=__min(len,bi->len - bi->offset);
	memcpy(buf,bi->buf + bi->offset,minLen);
	if((bi->offset+=minLen) >= bi->len) {
		delete bi->buf;
		scopeLock l(mutex);
		fdbuffers.erase(sockfd);
	}
	return minLen;
}
extern "C" ssize_t recv(int sockfd, void *buf, size_t len, int flags) {
	if(len<=0) return 0;
	if(prev_recv==NULL) {
		prev_recv=(recv_def)dlsym(RTLD_NEXT, "recv");
	}
	bufferInfo* bi=getBufferInfo(sockfd);
	if(bi==NULL) return prev_recv(sockfd,buf,len,flags);
	return fixRead(bi,sockfd,buf,len);
}
extern "C" ssize_t read(int fd, void *buf, size_t len) {
	if(len<=0) return 0;
	if(prev_read==NULL) {
		prev_read=(read_def)dlsym(RTLD_NEXT, "read");
	}
	bufferInfo* bi=getBufferInfo(fd);
	if(bi==NULL) return prev_read(fd,buf,len);
	return fixRead(bi,fd,buf,len);
}
int shutdown(int sockfd, int how) {
	if(prev_shutdown==NULL) {
		prev_shutdown=(shutdown_def)dlsym(RTLD_NEXT, "shutdown");
	}
	bool isListen;
	{
		scopeLock l(mutex);
		if(fds.count(sockfd)>0) {
			fprintf(stderr,"attempting to shut down listening fd %i\n",sockfd);
			return 0;
		}
	}
	return prev_shutdown(sockfd,how);
}
int close(int fd) {
	if(prev_close==NULL) {
		prev_close=(close_def)dlsym(RTLD_NEXT, "close");
	}
	{
		scopeLock l(mutex);
		if(fds.count(fd)>0)
			fds.erase(fd);
	}
	return prev_close(fd);
}
int dup2(int oldfd, int newfd) {
	if(prev_dup2==NULL) {
		prev_dup2=(dup2_def)dlsym(RTLD_NEXT, "dup2");
	}
	{
		scopeLock l(mutex);
		if(fds.count(oldfd)>0)
			fds.insert(newfd);
		else if(fds.count(newfd)>0)
			fds.erase(newfd);
	}
	return prev_dup2(oldfd,newfd);
}
int dup3(int oldfd, int newfd, int flags) {
	if(prev_dup3==NULL) {
		prev_dup3=(dup3_def)dlsym(RTLD_NEXT, "dup3");
	}
	{
		scopeLock l(mutex);
		if(fds.count(oldfd)>0)
			fds.insert(newfd);
		else if(fds.count(newfd)>0)
			fds.erase(newfd);
	}
	return prev_dup3(oldfd,newfd,flags);
}


