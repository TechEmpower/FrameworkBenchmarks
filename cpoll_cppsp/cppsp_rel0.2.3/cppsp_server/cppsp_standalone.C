/*
 This program is free software: you can redistribute it and/or modify
 it under the terms of the GNU General Public License as published by
 the Free Software Foundation, either version 3 of the License, or
 (at your option) any later version.

 This program is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 GNU General Public License for more details.

 You should have received a copy of the GNU General Public License
 along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * */
#include <cpoll/cpoll.H>
#include <unistd.h>
#include <iostream>
#include <signal.h>
#include <cppsp/page.H>
#include <cppsp/cppsp_cpoll.H>
#include <cppsp/common.H>
#include <assert.h>
#include <sys/syscall.h>	//SYS_gettid
#include "server.C"
#define PRINTSIZE(x) printf("sizeof("#x") = %i\n",sizeof(x))
#define printerr(x, ...) fprintf(stderr, "\x1B[41;1;33m" x "\x1B[0;0;0m\n", ##__VA_ARGS__)
#define printinfo(x, ...) fprintf(stderr, "\x1B[1;1;1m" x "\x1B[0;0;0m\n", ##__VA_ARGS__)
#ifndef SO_REUSEPORT
#define SO_REUSEPORT	15
#endif
#define CPPSP_LISTEN_BACKLOG 256

using namespace std;
using namespace CP;
using namespace cppsp;
using namespace RGC;
string rootDir;

void parseArgs(int argc, char** argv, const function<void(char*, const function<char*()>&)>& cb);

struct workerThread
{
	cppspServer::Server srv;
	vector<const char*> modules;
	Ref<CP::Socket> listenSock;
	union {
		pid_t pid;
		pthread_t thread;
	};
	int threadid;
	int cpu;	//id of cpu to pin to, or -1
	workerThread(Socket& sock): srv(rootDir.c_str()),
		listenSock(sock),cpu(-1){}
};
class handler1: public RGC::Allocator
{
public:
	Socket sock;
	cppspServer::handler h;
	handler1(cppspServer::Server& thr,CP::Poll& poll,HANDLE s,int d,int t,int p):
		sock(s,d,t,p),h(thr,poll,sock) {
			h.allocator=this;
	}
	void* alloc(int s) { return NULL; }
	void dealloc(void* ptr) {
		sock.~Socket();
		if(allocator==NULL)free(this);
		else allocator->dealloc(this);
	}
};
void pinToCPU(int cpu) {
	cpu_set_t s;
	CPU_ZERO(&s);
	CPU_SET(cpu,&s);
	if(sched_setaffinity((pid_t)syscall(SYS_gettid),sizeof(s),&s)!=0)
		perror("sched_setaffinity");
}
void* thread1(void* v) {
	workerThread& thr=*(workerThread*)v;
	cppspServer::Server& srv=thr.srv;
	Poll p;
	if(thr.cpu>=0) pinToCPU(thr.cpu);
	/*
	p.add(thr->efd);
	struct {
		Poll& p;
		serverThread* thr;
		void operator()(eventfd_t eventcount) {
			cppsp_request* req;
			while((req=thr->req_queue.beginDequeue())!=NULL) {
				Socket* sock=new Socket(req->fd,listensock.addressFamily,
					listensock.type,listensock.protocol);
				//printf("new socket: %p\n",sock);
				p.add(*sock);
				processRequest(*thr,p,*sock);
				sock->release();
				thr->req_queue.endDequeue();
			}
		}
	} cb {p, thr};
	thr->efd.repeatGetEvent(&cb);*/
	MemoryPool handlerPool(sizeof(handler1),256);
	struct {
		Poll& p;
		workerThread& thr;
		MemoryPool& handlerPool;
		int reqn;
		void operator()(HANDLE sock) {
			//printf("thread %i: accepted socket: %p (%i)\n",thr->threadid,sock,sock->handle);
			handler1* hdlr=new (handlerPool.alloc())
				handler1(thr.srv,p,sock,thr.listenSock->addressFamily,
					thr.listenSock->type,thr.listenSock->protocol);
			hdlr->allocator=&handlerPool;
			if(++reqn>10) {
				reqn=0;
				sched_yield();
			}
		}
	} cb {p, thr, handlerPool, 0};
	
	p.add(*thr.listenSock);
	Timer t((uint64_t)2000);
	struct {
		cppspServer::Server& srv;
		void operator()(int count) {
			srv.updateTime();
		}
	} cb1 {srv};
	t.setCallback(&cb1);
	p.add(t);
	
	int modsLeft;
	struct {
		int& modsLeft;
		workerThread& thr;
		Delegate<void(HANDLE)> cb;
		void operator()() {
			if(--modsLeft == 0) {
				thr.listenSock->repeatAcceptHandle(cb);
			}
		}
	} afterModuleLoad {modsLeft,thr,&cb};
	struct {
		const char* s;
		Delegate<void()> afterModuleLoad;
		void operator()(void*,exception* ex) {
			if(ex!=NULL) {
				fprintf(stderr,"error loading module %s: %s\n",s,ex->what());
				cppsp::CompileException* ce = dynamic_cast<cppsp::CompileException*>(ex);
				if (ce != NULL) {
					printf("%s\n",ce->compilerOutput.c_str());
				}
			}
			afterModuleLoad();
		}
	} moduleCB[thr.modules.size()];
	modsLeft=thr.modules.size();
	for(int ii=0;ii<(int)thr.modules.size();ii++) {
		moduleCB[ii].s=thr.modules[ii];
		moduleCB[ii].afterModuleLoad=&afterModuleLoad;
		thr.srv.loadModule(p,thr.modules[ii],&moduleCB[ii]);
	}
	if(thr.modules.size()==0) thr.listenSock->repeatAcceptHandle(&cb);
	p.loop();
	return NULL;
}

CP::Socket listensock;
int main(int argc, char** argv) {
	{
		char cwd[255];
		if(getcwd(cwd,255)==NULL) throw runtime_error(strerror(errno));
		rootDir=cwd;
	}
	string listen="0.0.0.0:80";
	int threads=-1;
	bool f0rk=false;
	vector<string> cxxopts;
	vector<const char*> modules;
	bool reusePort=true;
	bool setAffinity=false;
	try {
		parseArgs(argc, argv,
				[&](char* name, const std::function<char*()>& getvalue)
				{
					if(name==NULL) goto help;
					if(strcmp(name,"r")==0) {
						rootDir=getvalue();
					} else if(strcmp(name,"c")==0) {
						cxxopts.push_back(getvalue());
					} else if(strcmp(name,"g")==0) {
						cppsp::gxx=getvalue();
					} else if(strcmp(name,"l")==0) {
						listen=getvalue();
					} else if(strcmp(name,"t")==0) {
						threads=atoi(getvalue());
					} else if(strcmp(name,"m")==0) {
						modules.push_back(getvalue());
					} else if(strcmp(name,"f")==0) {
						f0rk=true;
					} else if(strcmp(name,"s")==0) {
						reusePort=false;
					} else if(strcmp(name,"a")==0) {
						setAffinity=true;
					} else {
					help:
						fprintf(stderr,"usage: %s [options]...\noptions:\n"
						"\t-l <host:port>: listen on specified host:port (default: 0.0.0.0:80)\n"
						"\t-g <option>: specify the C++ compiler (default: g++)\n"
						"\t-c <option>: specify a compiler option to be passed to g++\n"
						"\t-m <path>: load a cppsp module (path is relative to root)\n"
						"\t-r <root>: set root directory (must be absolute) (default: $(pwd))\n"
						"\t-t <threads>: # of worker processes/threads to start up (default: sysconf(_SC_NPROCESSORS_CONF))\n"
						"\t-f: use multi-processing (forking) instead of multi-threading (pthreads)\n"
						"\t-a: automatically set cpu affinity for the created worker threads/processes\n",argv[0]);
						exit(1);
					}
				});
	} catch(exception& ex) {
		printerr("error: %s\nspecify -? for help",ex.what());
		return 1;
	}
	printinfo("specify -? for help");
	auto i=listen.find(':');
	if(i==string::npos) throw runtime_error("expected \":\" in listen");
	
	int cpus=(int)sysconf(_SC_NPROCESSORS_CONF);
	if(threads<0)threads=cpus;
	if(setAffinity) {
		if(threads > cpus && (threads%(int)sysconf(_SC_NPROCESSORS_CONF) != 0)) {
			printerr("warning: cpu affinity is to be set; thread count larger than and not divisible by cpu count");
		}
	}
	
	EndPoint* ep=NULL;
	struct {
		bool& reusePort;
		void operator()(int s) {
			int optval = 1;
			reusePort=(setsockopt(s, SOL_SOCKET, SO_REUSEPORT, &optval, sizeof(optval))==0);
		}
	} initsock {reusePort};
	if(reusePort)
		listensock.bind(listen.substr(0,i).c_str(),
			listen.substr(i + 1, listen.length() - i - 1).c_str(), AF_UNSPEC, SOCK_STREAM,0,0,&initsock);
	else
		listensock.bind(listen.substr(0,i).c_str(),
			listen.substr(i + 1, listen.length() - i - 1).c_str(), AF_UNSPEC, SOCK_STREAM);
	if(reusePort) {
		printinfo("using SO_REUSEPORT");
		ep=listensock.getLocalEndPoint();
	} else {
		printerr("NOT using SO_REUSEPORT");
		listensock.listen(CPPSP_LISTEN_BACKLOG);
	}
	
	//p.add(listensock);
	PRINTSIZE(CP::Socket);
	PRINTSIZE(cppspServer::handler);
	PRINTSIZE(handler1);
	if(f0rk) printinfo("starting %i processes",threads);
	else printinfo("starting %i threads",threads);
	workerThread* th=(workerThread*)new char[sizeof(workerThread)*threads];
	for(int i=0;i<threads;i++) {
		int cpu=i%cpus;
		Socket* newsock;
		if(reusePort) {
			newsock=new Socket(listensock.addressFamily, listensock.type, listensock.protocol);
			int optval = 1;
			assert(setsockopt(newsock->handle, SOL_SOCKET, SO_REUSEPORT, &optval, sizeof(optval))==0);
			newsock->bind(*ep);
			newsock->listen(CPPSP_LISTEN_BACKLOG);
		} else {
			newsock=new Socket(f0rk ? listensock.handle : dup(listensock.handle),
				listensock.addressFamily, listensock.type, listensock.protocol);
		}
		workerThread& tmp=*(new (th+i) workerThread(*newsock));
		tmp.cpu=(setAffinity?cpu:-1);
		newsock->release();
		CXXOpts(tmp.srv.mgr)=cxxopts;
		tmp.modules=modules;
		tmp.threadid=i+1;
		if(threads==1) {
			thread1(&tmp);
			return 0;
		}
		if(f0rk) {
			pid_t pid=fork();
			if(pid==0) {
				tmp.pid=getpid();
				srand(int(tmp.pid)^(int)time(NULL));
				thread1(&tmp);
				return 0;
			} else if(pid>0) {
				tmp.pid=pid;
				//delete newsock;
			} else {
				perror("fork");
				return 1;
			}
		} else {
			if (pthread_create(&tmp.thread, NULL, thread1, &tmp) != 0) {
				throw runtime_error(strerror(errno));
			}
		}
	}
	if(f0rk) {
		static workerThread* _threads;
		static int _threadcount;
		struct sig_handler
		{
			static void a(int sig) {
				for(int i=0;i<_threadcount;i++) {
					kill(_threads[i].pid, 9);
				}
				exit(0);
			}
		};
		_threads=th;
		_threadcount=threads;
		struct sigaction sa;
		sa.sa_handler = &sig_handler::a;
		sigemptyset(&sa.sa_mask);
		sigaction(SIGINT, &sa, NULL);
		sigaction(SIGTERM, &sa, NULL);
		sigaction(SIGSEGV, &sa, NULL);
	}
	while(1)sleep(3600);
}
void parseArgs(int argc, char** argv, const function<void(char*, const function<char*()>&)>& cb) {
	int i = 1;
	function<char*()> func = [&]()->char*
	{
		if(i+1>=argc)throw logic_error(string(argv[i])+" requires an argument");
		return argv[(++i)];
	};
	for (; i < argc; i++) {
		if (argv[i][0] == '\x00') continue;
		if (argv[i][0] == '-') {
			cb(argv[i] + 1, func);
		} else {
			cb(NULL, [argv,i]()
			{	return argv[i];});
		}
	}
}

