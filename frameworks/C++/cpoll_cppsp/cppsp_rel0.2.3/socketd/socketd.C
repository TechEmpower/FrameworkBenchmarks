/*
 * config.cpp
 *
 *  Created on: 2011-05-20
 *      Author: xaxaxa
 *
 *
 */
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
#define _ISOC11_SOURCE
#include "include/socketd_internal.H"
#include <cpoll/sendfd.H>
#include <cpoll/statemachines.H>
#include <stdexcept>
#include <unistd.h>
#include <stdint.h>
#include <rgc.H>
#include <signal.h>
#include <tuple>
#include <ctype.h>
#include <delegate.H>
#include <set>
#include <stdlib.h>
#include <math.h>

#define PRINTSIZE(x) printf("sizeof("#x") = %i\n",sizeof(x))
#define SOCKETD_READBUFFER 256

//maximum length of user-controlled data on the stack (for example, http header names)
//in order to prevent stack overflow attacks
#define SOCKETD_STACKBUFFER 256
using namespace std;
using namespace CP;
//8: debug; 5: info; 3: warn; 2: err; 1: fatal
#define SOCKETD_DEBUG(LEVEL, ...) if(LEVEL<=5)printf(__VA_ARGS__)
//#define SOCKETD_DEBUG(...) /* __VA_ARGS__ */
namespace socketd
{
	static int asdf = 0;
	//static const int rBufSize = 4096;
	//static const int rLineBufSize = 512;
	void spawnApp(vhost* vh, CP::Poll& p, string exepath, int threadID, int i);
	bool comparePath(const char* conf, int confLen, const char* path, int pathLen) {
		SOCKETD_DEBUG(10, "comparePath: conf=%s; path=%s\n", string(conf, confLen).c_str(),
				string(path, pathLen).c_str());
		//cout << string(path, pathLen) << endl;
		if (confLen == pathLen && memcmp(conf, path, confLen) == 0) {
			/*cout << "matched (exact): " << string(path, pathLen) << " against " << string(conf, confLen)
			 << endl;*/
			return true;
		}
		if (pathLen > confLen && memmem(path, pathLen, conf, confLen) == path
				&& (confLen == 1 || path[confLen] == '/')) {
			/*cout << "matched (substring): " << string(path, pathLen) << " against " << string(conf, confLen)
			 << endl;*/
			return true;
		}
		//
		return false;
	}
	bool compareHost(const char* conf, int confLen, const char* host, int hostLen) {
		if (confLen == hostLen && memcmp(conf, host, confLen) == 0) {
			return true;
		}
		if (confLen >= 1 && conf[0] == '*') {
			if (((const char*) memmem(host, hostLen, conf + 1, confLen - 1)) - host
					== (hostLen - (confLen - 1))) {
				return true;
			}
		}
		return false;
	}
	SocketDException::SocketDException() :
			message(strerror(errno)), number(errno) {
	}
	SocketDException::SocketDException(int32_t number) :
			message(strerror(number)), number(number) {
	}
	SocketDException::SocketDException(string message, int32_t number) :
			message(message), number(number) {
	}
	SocketDException::~SocketDException() throw () {
	}
	const char* SocketDException::what() const throw () {
		return message.c_str();
	}
	char* strupper(char* s, int len) {
		for (char* p = s; *p; ++p)
			*p = toupper(*p);
		return s;
	}
	char* strlower(char* s, int len) {
		for (char* p = s; *p; ++p)
			*p = tolower(*p);
		return s;
	}
	bool compareStringCI(const char* s1, const char* s2, int l) {
		for (int i = 0; i < l; i++) {
			if (tolower(s1[i]) != tolower(s2[i])) return false;
		}
		return true;
	}
	bool compareStringCI(const char* s1, int l1, const char* s2) {
		int l2 = strlen(s2);
		if (l1 != l2) return false;
		return compareStringCI(s1, s2, l1);
	}

	int& getCurProcess(vhost* vh, socketd* This, int threadID) {
		uint8_t* data = vh->perCPUData[threadID];
		uint8_t* tmp = data + sizeof(appConnection*) * vh->_processes;
		return *(int*) tmp;
	}
	appConnection* getConn(vhost* vh, int threadID, int i) {
		uint8_t* data = vh->perCPUData[threadID];
		uint8_t* tmp = data + sizeof(appConnection*) * i;
		return *(appConnection**) tmp;
	}
	void setConn(vhost* vh, int threadID, int i, appConnection* c) {
		uint8_t* data = vh->perCPUData[threadID];
		uint8_t* tmp = data + sizeof(appConnection*) * i;
		appConnection*& conn = *(appConnection**) tmp;
		if (conn != NULL) conn->release();
		conn = c;
		if (conn != NULL) conn->retain();
	}
	struct connectionInfo
	{
		socketd* This;
		listen* l;
		CP::Socket s;
		CP::Poll* p;
		vhost* tmp_vh;
		appConnection* tmpptr;
		bool* deletionFlag;
		//CP::streamReader* sr;
		char _sr[sizeof(CP::newPersistentStreamReader)];

		//int lineBufLen;
		const char* httpPath;
		const char* httpHost;
		int httpPathLength;
		int httpHostLength;

		int tries;
		//0: none; 1: reqLine; 2: headers
		int readTo;
		int pos;
		int threadID;
		int processIndex;

		bool firstLine;
		bool reading;
		bool cancelread;
		bool shouldDelete;
		bool streamReaderInit;

		int& getCurProcess(vhost* vh) {
			return ::socketd::getCurProcess(vh, This, threadID);
		}
		appConnection* getConn(vhost* vh, int i) {
			uint8_t* data = vh->perCPUData[threadID];
			uint8_t* tmp = data + sizeof(appConnection*) * i;
			return *(appConnection**) tmp;
		}
		void setConn(vhost* vh, int i, appConnection* c) {
			uint8_t* data = vh->perCPUData[threadID];
			uint8_t* tmp = data + sizeof(appConnection*) * i;
			appConnection*& conn = *(appConnection**) tmp;
			if (conn != NULL) conn->release();
			conn = c;
			if (conn != NULL) conn->retain();
		}
		connectionInfo(int fd, int d, int t, int p) :
				s(fd, d, t, p), deletionFlag(NULL), tries(0), readTo(0), pos(0), processIndex(-1),
						shouldDelete(false), streamReaderInit(false) {
		}
		void startRead();
		void checkMatch();
		void startSocketRead();
		void socketReadCB(int r) {
			SOCKETD_DEBUG(9, "got %i bytes of data from client socket\n", r);
			CP::newPersistentStreamReader* sr = (CP::newPersistentStreamReader*) _sr;
			if (r > 0) {
				bool d(false);
				deletionFlag = &d;
				sr->endPutData(r);
				newPersistentStreamReader::item it;
				if (sr->process(it)) readCB((uint8_t*) it.data.data(), it.data.length());
				if (d) return;
				deletionFlag = NULL;
			}

			reading = false;
			if (shouldDelete) {
				delete this;
				return;
			}
			if (cancelread) return;
			if (r <= 0) {
				String s = sr->getBufferData();
				sr->clearBuffer();
				readCB((uint8_t*) s.data(), s.length());
				return;
			}
			startSocketRead();
		}
		void processLine(uint8_t* buf, int len) {
			//uint8_t* lineBuf = ((uint8_t*) sr) + CP::streamReader_getSize() + rBufSize;
			uint8_t* lineBuf = buf;
			int lineBufLen = len;
			SOCKETD_DEBUG(10, "got line: %s\n", string((const char* )lineBuf, lineBufLen).c_str());
			//printf("got line: ");
			//fflush(stdout);
			//write(1, lineBuf, lineBufLen);
			//printf("\n");
			if (len <= 0) goto fail;

			if (firstLine) {
				firstLine = false;
				uint8_t* tmp = (uint8_t*) memchr(lineBuf, ' ', lineBufLen);
				if (tmp == NULL) goto fail;
				tmp++;
				if (lineBuf + lineBufLen - tmp <= 0) goto fail;
				uint8_t* tmp1 = (uint8_t*) memchr(tmp, ' ', lineBuf + lineBufLen - tmp);
				if (tmp1 == NULL) goto fail;
				const char* path = (const char*) tmp;
				int pathLen = tmp1 - tmp;
				if (pathLen <= 0) goto fail;
				pos = 1;
				httpPath = path;
				httpPathLength = pathLen;
				SOCKETD_DEBUG(10, "got httpPath: %s\n", string(httpPath, httpPathLength).c_str());
				checkMatch();
				return;
			}

			const uint8_t* tmp;
			const uint8_t* end;
			end = buf + len;
			tmp = (const uint8_t*) memchr(buf, ':', len);
			if (tmp == NULL) goto cont;
			int i;
			i = tmp - buf;
			if (i > SOCKETD_STACKBUFFER) goto fail;
			if (compareStringCI((const char*) buf, i, "host")) {
				tmp++;
				while (tmp < end && *tmp == ' ')
					tmp++;
				if (tmp >= end) goto fail;
				httpHost = (const char*) tmp;
				httpHostLength = end - tmp;
				SOCKETD_DEBUG(10, "got httpHost: %s\n", string(httpHost, httpHostLength).c_str());
				pos = 2;
				checkMatch();
				return;
			}
			cont: startRead();
			return;
			fail: delete this;
		}
		void readCB(uint8_t* buf, int len) {
			/*uint8_t* lineBuf;
			 if (len <= 0) goto aaa;
			 //overflowed the line buffer
			 if (lineBufLen + len > rLineBufSize) goto fail;
			 lineBuf = ((uint8_t*) sr) + CP::streamReader_getSize() + rBufSize;
			 memcpy(lineBuf + lineBufLen, buf, len);
			 lineBufLen += len;
			 aaa: if (last) {
			 cancelread = true;
			 processLine();
			 }
			 return;
			 fail: delete this;*/
			cancelread = true;
			processLine(buf, len);
		}
		inline int connIndex(vhost* vh) {
			return threadID * vh->_processes + processIndex;
		}

		void attachmentCB(bool b) {
			if (b) {
				SOCKETD_DEBUG(8, "received acknownedgement for connection %p (with attachment)\n",
						this);
				delete this;
			} else {
				do_transfer(tmp_vh);
			}
		}
		void appCB(bool b) {
			if (b) {
				SOCKETD_DEBUG(8, "received acknownedgement for connection %p\n", this);
				delete this;
			} else {
				if (tmpptr == getConn(tmp_vh, processIndex)) {
					getConn(tmp_vh, processIndex)->shutDown();
					setConn(tmp_vh, processIndex, NULL);
				}
				do_transfer(tmp_vh);
			}
		}
		//transfer socket to application
		void do_transfer(vhost* vh) {
			//cout << "do_transfer" << endl;
			SOCKETD_DEBUG(8, "do_transfer (%p)\n", this);
			retry: if ((++tries) > 3) {
				SOCKETD_DEBUG(3, "exceeded 3 tries for connection %p\n", this);
				if (reading) shouldDelete = true;
				else delete this;
				return;
			}
			if (processIndex < 0) {
				processIndex = (getCurProcess(vh)++) % vh->_processes;
			}
			if (getConn(vh, processIndex) == NULL && vh->exepath.length() > 0) {
				spawnApp(vh, *p, vh->exepath, threadID, processIndex);
			}
			uint8_t* buf;
			int bufLen;
			if (streamReaderInit) {
				CP::newPersistentStreamReader* sr = (CP::newPersistentStreamReader*) _sr;
				String s = sr->getHistory();
				buf = (uint8_t*) s.data();
				bufLen = s.length();
			} else {
				buf = NULL;
				bufLen = 0;
			}
			if (vh->attachmentConn() != NULL) {
				tmp_vh = vh;
				int r = vh->attachmentConn->passConnection(&s, NULL, 0,
						appConnection::passConnCB(&connectionInfo::attachmentCB, this));
				if (r == 1) { //fail
					goto aaaaa;
				} else if (r == 0) { //success
					SOCKETD_DEBUG(8, "connection %p pre-succeeded (with attachment)\n", this);
					delete this;
					return;
				} else return;
			}
			aaaaa: if (getConn(vh, processIndex) != NULL) {
				//cout << "vh->conn() != NULL" << endl;
				appConnection* tmpptr = getConn(vh, processIndex);
				this->tmp_vh = vh;
				this->tmpptr = tmpptr;
				SOCKETD_DEBUG(8, "bufLen=%i\n", bufLen);
				int r = tmpptr->passConnection(&s, buf, bufLen,
						appConnection::passConnCB(&connectionInfo::appCB, this));
				if (r == 1) {
					//application possibly dead; respawn
					tmpptr->shutDown();
					if (tmpptr == getConn(vh, processIndex)) setConn(vh, processIndex, NULL);
					goto retry;
				} else if (r == 0) {
					SOCKETD_DEBUG(8, "connection %p pre-succeeded\n", this);
					delete this;
					return;
				} else return;
			} else {
				//no handler found; reset connection
				SOCKETD_DEBUG(5, "no handler for connection %p\n", this);
				delete this;
			}

		}
		void process() {
			SOCKETD_DEBUG(9, "connectionInfo::process()\n");
			checkMatch();
		}

		~connectionInfo() {
			SOCKETD_DEBUG(9, "~connectionInfo (%p)\n", this);
			//s.release();
			if (streamReaderInit) {
				CP::newPersistentStreamReader* sr = (CP::newPersistentStreamReader*) _sr;
				sr->~newPersistentStreamReader();
			}
			if (deletionFlag != NULL) *deletionFlag = true;
		}
	};
	void connectionInfo::startSocketRead() {
		if (reading) return;
		CP::newPersistentStreamReader* sr = (CP::newPersistentStreamReader*) _sr;
		auto tmp = sr->beginPutData(SOCKETD_READBUFFER);
		SOCKETD_DEBUG(9, "attempting to read %i bytes of data from client socket\n",
				SOCKETD_READBUFFER);
		reading = true;
		s.read(tmp.data(), SOCKETD_READBUFFER, CP::Callback(&connectionInfo::socketReadCB, this));
	}
	void connectionInfo::checkMatch() {
		//figure out what needs to be read to decide which binding to use

		//0: none; 1: reqLine; 2: headers
		//int readTo = 0;
		if (pos < readTo) {
			startRead();
			return;
		}
		SOCKETD_DEBUG(9, "bindings.size() = %i\n", This->bindings.size());

		for (uint32_t i = 0; i < This->bindings.size(); i++) {
			SOCKETD_DEBUG(9, "This->bindings[i]->listenID = %i\n", This->bindings[i]->listenID);
			if (!(This->bindings[i]->matchLevel & This->bindings[i]->match_listenID)
					|| This->bindings[i]->listenID == l->id) {
				if (This->bindings[i]->matchLevel & binding::match_httpPath) {
					if (pos < 1) {
						readTo = 1;
						break;
					} else {
						if (comparePath(This->bindings[i]->httpPath.data(),
								This->bindings[i]->httpPath.length(), httpPath, httpPathLength)) {
							goto matched_httpPath;
						} else continue;
					}
				} else {
					matched_httpPath: if (This->bindings[i]->matchLevel & binding::match_httpHost) {
						if (pos < 2) {
							readTo = 2;
							break;
						} else {
							if (comparePath(This->bindings[i]->httpHost.data(),
									This->bindings[i]->httpHost.length(), httpHost, httpHostLength)) {
								goto matched_httpHost;
							} else continue;
						}
					} else {
						matched_httpHost: do_transfer(This->bindings[i]->vh);
						return;
					}
				}
			}
		}
		SOCKETD_DEBUG(9, "readTo=%i pos=%i\n", readTo, pos);
		if (readTo > pos) {
			if (pos == 0) {
				//initialize streamReader
				CP::newPersistentStreamReader* sr;
				sr = new (_sr) CP::newPersistentStreamReader(SOCKETD_READBUFFER);
				streamReaderInit = true;
				//if (sr == NULL) goto fail;
				//CP::streamReader_init(sr, rBufSize);
				firstLine = true;
				reading = false;
				p->add(s);
			}
			startRead();
		} else goto fail;
		return;
		fail: if (reading) shouldDelete = true;
		else delete this;
	}
	void connectionInfo::startRead() {
		CP::newPersistentStreamReader* sr = (CP::newPersistentStreamReader*) _sr;
		sr->readUntilString("\r\n", 2);
		newPersistentStreamReader::item it;
		if (sr->process(it)) readCB((uint8_t*) it.data.data(), it.data.length());
		else {
			cancelread = false;
			startSocketRead();
		}
	}
	appConnection::appConnection() {
	}

	appConnection::~appConnection() {
//if(vh!=NULL && vh->conn==this) vh->conn=NULL;
	}
	struct appConnection_unix: public appConnection
	{
		RGC::Ref<CP::Socket> unixsock;

//not yet acknowledged
		map<int64_t, passConnCB> pendingConnections;
		int64_t maxID;
		protocolHeader buf;
		prot_ackConnection buf1;
		CP::Poll& p;
		socketd* sd;
		set<vhost*> bound_vhosts;
		pid_t pid;
//char sbuf[sizeof(protocolHeader)+sizeof(prot_handleConnection)];
		bool dead;
		bool down;

		virtual void shutDown() {
			if (!down) {
				down = true;
				unixsock->close();
				kill(pid, 15);
			}
		}
		void die(int64_t ignoreID) {
			if (dead) return;
			//throw 5;
			dead = true;
			for (auto it = pendingConnections.begin(); it != pendingConnections.end(); it++) {
				if ((*it).first != ignoreID) (*it).second(false);
			}
			pendingConnections.clear();
			shutDown();
		}
		void startRead();
		void ackConnectionCB(int r) {
			if (r <= 0) {
				die(0);
			}
			if (dead) {
				release();
				return;
			}
			//printf("%i\n",buf1.id);
			auto it = pendingConnections.find(buf1.id);
			if (it != pendingConnections.end()) {
				(*it).second(buf1.success);
				pendingConnections.erase(it);
			}
			startRead();
		}
		void readCB(int r) {
			if (r <= 0) {
				SOCKETD_DEBUG(5, "application died; r=%i; errno: %s\n", r, strerror(errno));
				die(0);
			}
			if (dead) {
				release();
				return;
			}
			switch (buf.type) {
				case protocolHeader::ackConnection:
				{
					unixsock->read(&buf1, sizeof(buf1),
							CP::Callback(&appConnection_unix::ackConnectionCB, this));
					break;
				}
				default:
					startRead();
					break;
			}
		}
		appConnection_unix(vhost* vh, CP::Poll& p, string exepath) :
				maxID(0), p(p), pid(0), dead(false), down(false) {
			int socks[2];
			if (socketpair(AF_UNIX, SOCK_STREAM, 0, socks) < 0) {
				throw runtime_error(strerror(errno));
			}
			if (vh->_ipcBufSize > 0) {
				int n;
				unsigned int n_size = sizeof(n);
				n = vh->_ipcBufSize;
				setsockopt(socks[0], SOL_SOCKET, SO_RCVBUF, (void *) &n, n_size);
				setsockopt(socks[0], SOL_SOCKET, SO_SNDBUF, (void *) &n, n_size);
				setsockopt(socks[1], SOL_SOCKET, SO_RCVBUF, (void *) &n, n_size);
				setsockopt(socks[1], SOL_SOCKET, SO_SNDBUF, (void *) &n, n_size);
			}
			pid_t pid = fork();
			if (pid < 0) throw runtime_error(strerror(errno));
			else if (pid == 0) {
				//child
				close(socks[0]);
				if (socks[1] != 3) {
					dup2(socks[1], 3); //fd 3
					close(socks[1]);
				}
				setenv("SOCKETD_FD", "3", 1);
				if (vh->preload) {
					setenv("LD_PRELOAD", socketd_proxy, 1);
				}
				if (vh->useShell) execlp("/bin/sh", "/bin/sh", "-c", exepath.c_str(),
						(const char*) NULL);
				else execlp(exepath.c_str(), exepath.c_str(), (const char*) NULL);
				_exit(1);
			} else {
				//parent
				close(socks[1]);
				this->pid = pid;
				//getsockopt(socks[0], SOL_SOCKET, SO_RCVBUF, (void *) &n, &n_size);
				//SOCKETD_DEBUG(8, "unix socket receive buffer size: %i\n", n);

				unixsock = RGC::newObj<CP::Socket>(socks[0], AF_UNIX, SOCK_STREAM, 0);
				p.add(*unixsock);
				//printf("asdfg %i\n", ++asdf);
				retain();
				startRead();
			}
		}
		appConnection_unix(CP::Socket* sock, CP::Poll& p, socketd* sd) :
				maxID(0), p(p), sd(sd), dead(false), down(false) {

		}
		virtual int passConnection(CP::Socket* s, void* buffer, int buflen, const passConnCB& cb) {
			if (dead) return 1;
			//printf("passConnection\n");
			//s->retain();
			int64_t id = (++maxID);
			int r;
			{
				int len = sizeof(protocolHeader) + sizeof(prot_handleConnection);
				uint8_t* hdr[len];
				memset(hdr, 0, len);
				protocolHeader* ph = new (hdr) protocolHeader();
				prot_handleConnection* ph1 = new (ph + 1) prot_handleConnection();

				ph->type = protocolHeader::handleConnection;
				//printf("zxcv %lli\n",id);
				ph1->id = id;
				ph1->d = s->addressFamily;
				ph1->t = s->type;
				ph1->p = s->protocol;
				ph1->bufferLen = buflen;
				//socket has SOCK_NONBLOCK set, so regular send() won't block;
				//if the socket buffer is full, then the application is already
				//considered dead
				r = unixsock->sendAll(hdr, len, MSG_DONTWAIT);
				ph->~protocolHeader();
				ph1->~prot_handleConnection();
			}
			if (r <= 0) goto fail;
			if (sendfd(unixsock->handle, s->handle) < 0) goto fail;
			if (buflen > 0) if (unixsock->sendAll(buffer, buflen, MSG_DONTWAIT) <= 0) goto fail;
			//s->release();
			pendingConnections.insert( { id, cb });
			return 2;
			fail: //s->release();
			SOCKETD_DEBUG(1, "unix socket buffer overflow; %s\n", strerror(errno));
			die(id);
			return 1;
		}
		virtual ~appConnection_unix() {
		}

	};
	void appConnection_unix::startRead() {
		unixsock->read(&buf, sizeof(buf), CP::Callback(&appConnection_unix::readCB, this));
	}
	void spawnApp(vhost* vh, CP::Poll& p, string exepath, int threadID, int i) {
		setConn(vh, threadID, i, RGC::newObj<appConnection_unix>(vh, p, exepath));
	}

	struct socketd_execinfo;
	struct socketd_thread
	{
		socketd* This;
		socketd_execinfo* execinfo;
		pthread_t thr;
		int id;
	};
	struct socketd_execinfo
	{
		vector<socketd_thread> threads;
	};
	void* socketd_processorThread(void* v) {
		socketd_thread* th = (socketd_thread*) v;
		CP::Poll p;
		for (uint32_t i = 0; i < th->This->listens.size(); i++) {
			auto& l = th->This->listens[i];
			struct cb1
			{
				CP::Poll& poll;
				socketd_thread* th;
				listen& l;
				Socket s;
				cb1(Poll& poll, socketd_thread* th, listen& l) :
						poll(poll), th(th), l(l), s(l.socks[th->id], l.d, l.t, l.p) {
					s.repeatAcceptHandle(this);
					poll.add(s);
				}
				void operator()(HANDLE h) {
					connectionInfo* ci = new connectionInfo(h, l.d, l.t, l.p);
					ci->threadID = th->id;
					ci->This = th->This;
					ci->l = &l;
					SOCKETD_DEBUG(9, "req.l.id = %i\n", l.id);
					ci->p = &poll;
					ci->process();
				}
			}* cb = new cb1(p, th, l);
		}
		p.loop();
		printf("%i exited\n", th->id);
		return NULL;
	}
	void socketd::run() {
		PRINTSIZE(CP::Socket);
		PRINTSIZE(CP::newPersistentStreamReader);
		PRINTSIZE(connectionInfo);
//ignore SIGCHLD
		struct sigaction sa;
		sa.sa_handler = SIG_IGN;
		sigemptyset(&sa.sa_mask);
		sa.sa_flags = SA_RESTART; /* Restart system calls if
		 interrupted by handler */
		sigaction(SIGCHLD, &sa, NULL);

		CP::Poll p;
		//p.debug=true;
		this->bindings.clear();
		for (int i = 0; i < (int) vhosts.size(); i++) {
			vhosts[i]._processes = ceil(double(vhosts[i].processes) / threads);
			if (vhosts[i]._processes < 1) vhosts[i]._processes = 1;
		}

		perCPUData = new uint8_t*[threads];
		for (int i = 0; i < threads; i++) {
			int s = 0;
			for (int ii = 0; ii < (int) vhosts.size(); ii++) {
				s += sizeof(appConnection*) * vhosts[ii]._processes;
				s += sizeof(int) * 2;
			}
			int align = 64;
			if (s % align != 0) s = ((s / align) + 1) * align;
			uint8_t* tmp; // = aligned_alloc(64, s);
			if (posix_memalign((void**) &tmp, align, s) != 0) throw bad_alloc();
			memset(tmp, 0, s);
			s = 0;
			for (int ii = 0; ii < (int) vhosts.size(); ii++) {
				vhosts[ii].perCPUData.push_back(tmp + s);
				s += sizeof(appConnection*) * vhosts[ii]._processes;
				s += sizeof(int) * 2;
			}
		}
		for (uint32_t i = 0; i < vhosts.size(); i++) {
			for (uint32_t ii = 0; ii < vhosts[i].bindings.size(); ii++) {
				binding* tmp = &(vhosts[i].bindings[ii]);
				this->bindings.push_back(tmp);
				vhosts[i].bindings[ii].vh = &vhosts[i];
			}
			vhosts[i]._ipcBufSize = vhosts[i].ipcBufSize < 0 ? this->ipcBufSize : vhosts[i].ipcBufSize;
			vhosts[i].hasAttachments = false;
			//vhosts[i].conns.resize(nthreads * vhosts[i].processes);
			//vhosts[i].curProcess.resize(nthreads);
			vhosts[i].perCPUData.resize(threads);
		}
		for (uint32_t i = 0; i < extraBindings.size(); i++) {
			for (uint32_t ii = 0; ii < vhosts.size(); ii++) {
				if (vhosts[ii].name == extraBindings[i].vhostName) {
					extraBindings[i].vh = &vhosts[ii];
					break;
				}
			}
			binding* tmp = &(extraBindings[i]);
			this->bindings.push_back(tmp);
		}
		//start up unix listening socket
		/*if (unixAddress.length() > 0) {
		 CP::Socket* unixsock = new CP::Socket(AF_UNIX, SOCK_STREAM);
		 unixsock->bind(CP::UNIXEndPoint(unixAddress));
		 unixsock->listen(8);
		 unixsock->repeatAccept([](CP::Socket* s) {

		 });
		 }*/

		SOCKETD_DEBUG(9, "bindings.size() = %i\n", bindings.size());
		for (uint32_t i = 0; i < listens.size(); i++) {
			auto& l = listens[i];
			Socket tmp;
			tmp.bind(l.host.c_str(), l.port.c_str(), AF_UNSPEC, SOCK_STREAM);
			tmp.listen(l.backlog);
			l.d = tmp.addressFamily;
			l.t = tmp.type;
			l.p = tmp.protocol;
			l.socks.resize(threads);
			for (int ii = 0; ii < threads; ii++) {
				l.socks[ii] = dup(tmp.handle);
			}
		}
		socketd_execinfo execinfo;
		printf("this=%p\n", this);
		execinfo.threads.resize(threads);
		SOCKETD_DEBUG(3, "starting %i threads\n", threads);
		for (int i = 0; i < threads; i++) {
			socketd_thread& th = execinfo.threads[i];
			th.This = this;
			th.execinfo = &execinfo;
			th.id = i;
			//printf("%p %p\n",&th, &th1);
			if (pthread_create(&th.thr, NULL, socketd_processorThread, &th) != 0) {
				throw runtime_error(strerror(errno));
			}
		}
		while (true)
			sleep(3600);
	}
}
