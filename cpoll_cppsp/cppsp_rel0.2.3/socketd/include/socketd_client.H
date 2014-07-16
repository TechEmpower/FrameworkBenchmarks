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
/*
 * socketd_client.H
 *
 *  Created on: Mar 8, 2013
 *      Author: xaxaxa
 */

#ifndef SOCKETD_CLIENT_H_
#define SOCKETD_CLIENT_H_
#include <socketd.H>
#include <cpoll/cpoll.H>
#include <unistd.h>
#include <cpoll/sendfd.H>
#include <functional>
#include <sstream>
#define CONCAT(X) (((stringstream&)(stringstream()<<X)).str())
namespace socketd
{
	using namespace std;
	using namespace CP;
	class SocketProxy: public CP::Socket
	{
	public:
		uint8_t* buf;
		int bufPos, bufLen;
		SocketProxy(int fd, int d, int t, int p, int buflen) :
				CP::Socket(fd, d, t, p), bufPos(0), bufLen(buflen) {
			buf = new uint8_t[buflen];
		}
		int32_t tryFixRead(void* buf, int32_t len) {
			if (this->buf == NULL || bufPos >= bufLen) return -1;
			int32_t l = len > (bufLen - bufPos) ? (bufLen - bufPos) : len;
			if (l <= 0) return 0;
			memcpy(buf, this->buf + bufPos, l);
			bufPos += l;
			if (bufPos >= bufLen) {
				delete[] this->buf;
				this->buf = NULL;
			}
			return l;
		}
		int32_t read(void* buf, int32_t len) {
			int32_t r;
			if ((r = tryFixRead(buf, len)) == -1) return CP::Socket::read(buf, len);
			else return r;
		}
		int32_t recv(void* buf, int32_t len, int32_t flags = 0) {
			int32_t r;
			if ((r = tryFixRead(buf, len)) == -1) CP::Socket::recv(buf, len, flags);
			else {
				if (flags & MSG_WAITALL) {
					if (r < len) {
						int32_t tmp = CP::Socket::recv(((uint8_t*) buf) + r, len - r, flags);
						if (tmp > 0) r += tmp;
					}
				}
				return r;
			}
		}
		void read(void* buf, int32_t len, const Callback& cb, bool repeat = false) {
			int32_t r;
			if ((r = tryFixRead(buf, len)) == -1) CP::Socket::read(buf, len, cb, repeat);
			else {
				cb(r);
				if (repeat && r > 0) CP::Socket::read(buf, len, cb, true);
			}
		}
		void recv(void* buf, int32_t len, int32_t flags, const Callback& cb, bool repeat = false) {
			int32_t r;
			if ((r = tryFixRead(buf, len)) == -1) CP::Socket::recv(buf, len, flags, cb, repeat);
			else {
				//MSG_WAITALL is not supposed to be specified on an asynchoronous CP::Socket::recv() call

				/*
				 if (flags & MSG_WAITALL) {
				 if (r < len) {
				 CP::Socket::recv(((uint8_t*) buf) + r, len - r, flags,
				 [cb,r,repeat,buf,len,flags,this](int i)
				 {
				 int r1=r;
				 if(i>0)r1+=i;
				 cb(r1);
				 if(repeat)CP::Socket::recv(buf, len, flags, cb, true);
				 }, false);
				 return;
				 }
				 }*/
				cb(r);
				if (repeat) CP::Socket::recv(buf, len, flags, cb, true);
			}
		}
		~SocketProxy() {
			if (buf != NULL) delete[] buf;
		}
	};
	class socketd_client
	{
	public:
		CP::Poll& p;
		RGC::Ref<CP::Socket> sock;
		Delegate<void(socketd_client&, Socket*, int64_t id)> cb;
		protocolHeader ph;
		prot_handleConnection ph1;
		bool raw;
		/*vector<int> acks;
		 uint8_t* tmp;
		 int tmplen;
		 bool writing;
		 void startWrite() {
		 if(writing || acks.size()<=0)return;
		 int sz=sizeof(protocolHeader)+sizeof(prot_ackConnection);
		 int sz1=sz*acks.size();
		 if(tmplen<sz1) {
		 if(tmp!=NULL)free(tmp);
		 tmplen=sz1;
		 tmp=(uint8_t*)malloc(sz1);
		 }
		 for(int i=0;i<acks.size();i++) {
		 protocolHeader* ph=(protocolHeader*)(tmp+(sz*i));
		 prot_ackConnection* ack=(prot_ackConnection*)(ph+1);
		 ph->type=protocolHeader::ackConnection;
		 ack->id=acks[i];
		 ack->success=true;
		 }
		 acks.resize(0);
		 writing=true;
		 sock->write(tmp,sz1,[this](int r) {
		 writing=false;
		 if(r<=0)return;
		 startWrite();
		 });
		 }*/
		void startRead();
		void ack(int64_t id) {
			if (raw) return;
			protocolHeader ph;
			memset(&ph, 0, sizeof(ph));
			ph.type = protocolHeader::ackConnection;
			prot_ackConnection ack;
			memset(&ack, 0, sizeof(ack));
			ack.id = id;
			ack.success = true;
			if (this->sock->send(&ph, sizeof(ph), MSG_DONTWAIT) != sizeof(ph)) throw runtime_error(
					"unix socket buffer overflow");
			if (this->sock->send(&ack, sizeof(ack), MSG_DONTWAIT) != sizeof(ack)) throw runtime_error(
					"unix socket buffer overflow");
			//acks.push_back(id);
			//startWrite();
		}
		void handleConnectionCB(int r) {
			if (r <= 0) {
				cb(*this, (Socket*) NULL, 0);
				return;
			}
			int fd = recvfd(sock->handle);
			if (fd < 0) {
				cb(*this, (Socket*) NULL, 0);
				return;
			}
			CP::Socket* newsock;
			//printf("asdfg %i\n",ph1.bufferLen);
			if (ph1.bufferLen <= 0) {
				newsock = new CP::Socket(fd, ph1.d, ph1.t, ph1.p);
			} else {
				SocketProxy* tmps;
				tmps = new SocketProxy(fd, ph1.d, ph1.t, ph1.p, ph1.bufferLen);
				int r = sock->recv(tmps->buf, ph1.bufferLen, MSG_WAITALL);
				if (r <= 0) {
					cb(*this, (Socket*) NULL, 0);
					return;
				}
				newsock = tmps;
				/*char junk[ph1.bufferLen];
				 sock->recv(junk, ph1.bufferLen, MSG_WAITALL);
				 newsock = new CP::Socket(fd, ph1.d, ph1.t, ph1.p);*/
			}
			p.add(*newsock);
			int64_t id = ph1.id;
			//printf("aaaaa %lli %i %i %i\n",ph1.id, ph1.d, ph1.t, ph1.p);
			cb(*this, newsock, id);
			newsock->release();
			startRead();
		}
		void readCB(int r) {
			//printf("readCB: this=%p\n",(void*)this);
			if (r <= 0) {
				cb(*this, (Socket*) NULL, 0);
				return;
			}
			if (r != sizeof(ph)) throw runtime_error(
					CONCAT("attempting to read protocolHeader resulted in short read: r=" << r) );
			switch (ph.type) {
				case protocolHeader::handleConnection:
				{
					sock->read(&ph1, sizeof(ph1),
							CP::Callback(&socketd_client::handleConnectionCB, this));
					return;
				}
				default:
				{
					throw runtime_error(CONCAT("unrecognized protocolHeader.type " << ph.type) );
				}
			}
			startRead();
		}
		socketd_client(CP::Poll& p, const Delegate<void(socketd_client&, Socket*, int64_t)>& cb,
				CP::Socket* sock = NULL) :
				p(p), cb(cb), raw(false)/*, tmp(NULL),tmplen(0),writing(false)*/{
			if (sock == NULL) {
				char* listen = getenv("SOCKETD_LISTEN");
				if (listen != NULL) {
					const char* aaa = (const char*) memchr(listen, ':', strlen(listen));
					if (aaa == NULL) throw runtime_error("expected \":\" in SOCKETD_LISTEN");
					int i = aaa - listen;
					sock = RGC::newObj<CP::Socket>();
					sock->bind(string(listen, i).c_str(),
							string(listen + i + 1, strlen(listen) - i - 1).c_str(), AF_UNSPEC,
							SOCK_STREAM);
					p.add(*sock);
					struct CB1
					{
						socketd_client* This;
						Delegate<void(socketd_client&, Socket*, int64_t)> cb;
						int64_t id;
						void operator()(Socket* s) {
							This->p.add(*s);
							cb(*This, s, (++id));
							s->release();
						}
					}*cb1 = new CB1 { this, cb, 0 };
					sock->listen();
					raw = true;
					sock->repeatAccept(cb1);
					return;
				}
				char* tmp;
				tmp = getenv("SOCKETD_FD");
				if (tmp == NULL) throw logic_error("environment \"SOCKETD_FD\" not set");
				sock = RGC::newObj<CP::Socket>(atoi(tmp), AF_UNIX, SOCK_STREAM, 0);
				p.add(*sock);
			}
			this->sock = sock;
			startRead();
		}
	};
	void socketd_client::startRead() {
		//memset(&ph, 0, sizeof(ph));
		//printf("startRead: this=%p\n",(void*)this);
		sock->recv(&ph, sizeof(ph), 0, CP::Callback(&socketd_client::readCB, this));
	}

}
#endif /* SOCKETD_CLIENT_H_ */
