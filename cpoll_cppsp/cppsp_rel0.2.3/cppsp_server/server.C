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
using namespace std;
using namespace CP;
using namespace cppsp;
using namespace RGC;
#define rmb() /**/
#define wmb() /**/

namespace cppspServer
{
	#define CACHELINE_SIZE 64
	//currently not used
	template<class T> class RingBuffer
	{
	public:
		union {
			struct {
				T* items;
				int length;
			};
			char padding1[CACHELINE_SIZE];
		};
		union {
			int rpos;
			char padding2[CACHELINE_SIZE];
		};
		union {
			int wpos;
			char padding3[CACHELINE_SIZE];
		};
		
		RingBuffer(int length): length(length),rpos(0), wpos(0) {
			items=new T[length];
		}
		inline int __getlength(int i1, int i2, int wrap)
		{
			return (i2 < i1 ? i2 + wrap : i2) - i1;
		}
		inline bool canEnqueue()
		{
			return __getlength(rpos, wpos, (length*2)) < length;
		}
		inline bool canDequeue()
		{
			return __getlength(rpos, wpos, (length*2)) > 0;
		}
		T* beginEnqueue() {
			if(canEnqueue()) return items+(wpos%length);
			else return NULL;
		}
		void endEnqueue() {
			wmb();
			wpos=(wpos+1)%(length*2);
		}
		T* beginDequeue() {
			if(!canDequeue()) return NULL;
			rmb();
			return items+(rpos%length);
		}
		void endDequeue() {
			rpos=(rpos+1)%(length*2);
		}
	};
	class Server: public cppsp::Server {
	public:
		cppspManager* mgr;
		String root;
		Server(String root):mgr(cppspManager_new()),root(root) {
		}
		~Server() {
			cppspManager_delete(mgr);
		}
		
		void handleStaticRequest(String path, cppsp::Request& req, Response& resp, Delegate<void()> cb) override;
		void handleDynamicRequest(String path, cppsp::Request& req, Response& resp, Delegate<void()> cb) override;
		
		void loadPage(CP::Poll& p, String path, RGC::Allocator& a,
			Delegate<void(Page*, exception* ex)> cb) override {
			string tmp = mapPath(path.toSTDString());
			cppsp::loadPage(mgr, p, rootDir(), {tmp.data(), (int) tmp.length()}, &a, cb);
		}
		void loadPageFromFile(CP::Poll& p, String path, RGC::Allocator& a,
				Delegate<void(Page*, exception* ex)> cb) override {
			cppsp::loadPage(mgr, p, rootDir(), path, &a, cb);
		}
		void loadModule(CP::Poll& p, String path, 
			Delegate<void(void*, exception* ex)> cb) override {
			string tmp = mapPath(path.toSTDString());
			cppsp::loadModule(mgr, p, this, rootDir(), {tmp.data(), (int) tmp.length()}, cb);
		}
		void loadModuleFromFile(CP::Poll& p, String path, 
				Delegate<void(void*, exception* ex)> cb) override {
			cppsp::loadModule(mgr, p, this, rootDir(), path, cb);
		}
		String loadStaticPage(String path) override {
			string tmp = mapPath(path.toSTDString());
			return cppsp::loadStaticPage(mgr,{tmp.data(), (int) tmp.length()});
		}
		String loadStaticPageFromFile(String path) override {
			return cppsp::loadStaticPage(mgr,path);
		}
		String rootDir() override {
			return root;
		}
		cppspManager* manager() override {
			return mgr;
		}
		//this function needs to be called periodically to check for file modifications
		//otherwise auto re-compile will not work
		//generally call it every 2 seconds
		void updateTime() {
			cppsp::updateTime(mgr);
		}
	};
	class Request:public cppsp::CPollRequest
	{
	public:
		Request(CP::Socket& s, CP::StringPool* sp) :
			CPollRequest(s, sp) {
		}
		void* _handler;
	};
	//handles a single connection
	//just instantiate-and-forget; it will self-destruct when connection is closed
	struct handler:public RGC::Object {
		Allocator* alloc;
		Server& thr;
		CP::Poll& p;
		Socket& s;
		Page* page;
		StringPool sp;
		Request req;
		cppsp::Response resp;
		//Page* p;
		//MemoryStream ms;
		uint8_t* buf;
		String path;
		iovec iov[2];
		bool keepAlive;
		handler(Server& thr,CP::Poll& poll,Socket& s):thr(thr),
			p(poll),s(s), req(this->s,&sp),resp(this->s,&sp) {
			//printf("handler()\n");
			req._handler=this;
			poll.add(this->s);
			s.retain();
			if(req.readRequest({&handler::readCB, this})) readCB(true);
		}
		void readCB(bool success) {
			if(!success) {
				destruct();
				return;
			}
			auto it=req.headers.find("connection");
			if(it!=req.headers.end() && (*it).value=="close")keepAlive=false;
			else keepAlive=true;
			resp.headers.insert({"Connection", keepAlive?"keep-alive":"close"});
			resp.headers.insert({"Date", sp.addString(thr.mgr->curRFCTime)});
			
			thr.handleRequest(req,resp,{&handler::finalize,this});
		}
		void setPath(String p) {
			path.d=sp.beginAdd(p.length()+thr.root.length());
			path.len=cppsp::combinePathChroot(thr.root.data(),thr.root.length(),
				p.data(),p.length(),path.data());
			sp.endAdd(path.len);
		}
		static inline int itoa(int i, char* b) {
			static char const digit[] = "0123456789";
			char* p = b;
			p += (i==0?0:int(log10f(i))) + 1;
			*p = '\0';
			int l = p - b;
			do { //Move back, inserting digits as u go
				*--p = digit[i % 10];
				i = i / 10;
			} while (i);
			return l;
		}
		void handleStatic(String _path) {
			try {
				setPath(_path);
				String data=cppsp::loadStaticPage(thr.mgr,path);
				int bufferL = resp.buffer.length();
				{
					char* tmps = sp.beginAdd(16);
					int l = itoa(data.length(), tmps);
					sp.endAdd(l);
					resp.headers.insert({"Content-Length", { tmps, l }});
					StreamWriter sw(resp.buffer);
					resp.serializeHeaders(sw);
				}
				iov[0]= {resp.buffer.data()+bufferL, (size_t)(resp.buffer.length()-bufferL)};
				iov[1]= {data.data(), (size_t)data.length()};
				resp.outputStream->writevAll(iov, 2, { &handler::writevCB, this });
			} catch(exception& ex) {
				cppsp::handleError(&ex,resp,path);
				resp.flush( { &handler::flushCB, this });
			}
		}
		void handleDynamic(String _path) {
			setPath(_path);
			cppsp::loadPage(thr.mgr,p,thr.root,path,&sp,{&handler::loadCB,this});
		}
		void loadCB(Page* p, exception* ex) {
			//printf("loadCB()\n");
			if(ex!=NULL) {
				cppsp::handleError(ex,resp,path);
				resp.flush( { &handler::flushCB, this });
				goto doFinish;
			}
			{
				this->page=p;
				p->sp=&sp;
				//this->p=p;
				//p->filePath=path;
				p->request=&req;
				p->response=&resp;
				p->poll=&this->p;
				p->server=&thr;
				p->handleRequest({&handler::handleRequestCB,this});
				return;
			}
		doFinish:;
		}
		void sockReadCB(int r) {
			if(r<=0) {
				free(buf);
				destruct();
			}
		}
		void flushCB(Response& resp) {
			//s->shutdown(SHUT_WR);
			//release();
			finalize();
		}
		void writevCB(int i) {
			finalize();
		}
		void handleRequestCB() {
			page->destruct();
			page=nullptr;
			//s->shutdown(SHUT_WR);
			//release();
			//s->repeatRead(buf,sizeof(buf),{&handler::sockReadCB,this});
			finalize();
		}
		void finalize() {
			if(resp.closed) {
				destruct(); return;
			}
			sp.clear();
			if(keepAlive) {
				req.reset();
				resp.reset();
				if(req.readRequest({&handler::readCB,this})) readCB(true);
			} else {
				s.shutdown(SHUT_WR);
				buf=(uint8_t*)malloc(4096);
				s.repeatRead(buf,4096,{&handler::sockReadCB,this});
			}
		}
		~handler() {
			//printf("~handler()\n");
			s.release();
		}
	};
	void Server::handleStaticRequest(String path, cppsp::Request& req, Response& resp, Delegate<void()> cb) {
		cppspServer::Request& r=static_cast<cppspServer::Request&>(req);
		(*(handler*)r._handler).handleStatic(path);
	}
	void Server::handleDynamicRequest(String path, cppsp::Request& req, Response& resp, Delegate<void()> cb) {
		cppspServer::Request& r=static_cast<cppspServer::Request&>(req);
		(*(handler*)r._handler).handleDynamic(path);
	}
}
