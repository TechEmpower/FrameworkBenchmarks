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
 * page.H
 *
 *  Created on: Jan 26, 2013
 *      Author: xaxaxa
 */

#ifndef PAGE_H_
#define PAGE_H_
#include <rgc.H>
#include <map>
#include <cpoll/cpoll.H>
#include "stringutils.H"
#include "headercontainer.H"
using namespace std;
namespace cppsp
{
	class cppspManager;
	typedef CP::String String;
	/**
	 Represents an HTTP request and contains data from the request such as HTTP headers, querystrings, and HTTP POST data.
	 */
	class Request: public RGC::Object
	{
	public:
		Request(CP::Stream& inp, CP::StringPool* sp);
		virtual ~Request() {
		}
		/**
		 internal field do not use.
		 */
		RGC::Ref<CP::Stream> inputStream;
		/**
		 The per-request StringPool instance. You can allocate memory from this if it only need to persist for the duration of this http request. The StringPool is cleared after the page has finished executing.
		 */
		CP::StringPool* sp; //may be used to store headers, querystrings, and POST data
		typedef map<String, String, less<String>, CP::PoolAllocator<std::pair<const String, String> > > StringMap;
		CP::PoolAllocator<std::pair<const String, String> > alloc;
		/**
		 HTTP request headers.
		 */
		headerContainer headers;
		/**
		 Query strings.
		 */
		StringMap queryString;
		/**
		 POST data.
		 */
		StringMap form;
		/**
		 HTTP method (GET, POST, etc).
		 */
		String method;
		/**
		 HTTP request path (directly from the HTTP header; not file path; does not include querystrings).
		 */
		String path;
		//string httpVersion;

		/**
		 You don't need to call this manually; POST data is automatically read for all HTTP POST requests.
		 */
		virtual void readPost(Delegate<void(Request&)> cb)=0;
		/**
		 internal method do not use.
		 */
		void parsePost(String buf);
		/**
		 internal method do not use.
		 */
		virtual void reset();
	};
	/**
	 Represents an HTTP response.
	 */
	class Response: public RGC::Object
	{
	public:
		typedef Delegate<void(Response&)> Callback;
		Response(CP::Stream& out, CP::StringPool* sp);
		/**
		 internal field do not use.
		 */
		RGC::Ref<CP::Stream> outputStream;
		/**
		 internal field do not use.
		 */
		CP::MemoryStream buffer;
		/**
		 You can write data to the HTTP response body by writing to "output". Content-Length will be automatically calculated.
		 */
		CP::StreamWriter output;
		/**
		 internal field do not use.
		 */
		Callback _cb;
		/**
		 The per-request StringPool instance. You can allocate memory from this if it only need to persist for the duration of this http request. The StringPool is cleared after the page has finished executing.
		 */
		CP::StringPool* sp;
		//headerContainer2 headers;
		CP::PoolAllocator<std::pair<const String, String> > alloc;
		typedef map<String, String, less<String>, CP::PoolAllocator<std::pair<const String, String> > > StringMap;
		/**
		 HTTP response headers.
		 */
		StringMap headers;
		/**
		 HTTP response status (OK, Internal Server Error, etc).
		 */
		String statusName;
		/**
		 internal field do not use.
		 */
		iovec iov[2];
		/**
		 HTTP response status code (200, 500, etc).
		 */
		int statusCode;

		/**
		 internal field do not use.
		 */
		bool headersWritten;
		/**
		 internal field do not use.
		 */
		bool closed;
		/**
		 TODO: chunked output not yet implemented.
		 */
		bool sendChunked;
		/*virtual void doWriteHeaders();
		 void writeHeaders() {
		 if (!headersWritten) {
		 doWriteHeaders();
		 headersWritten = true;
		 }
		 }*/
		/**
		 internal method do not use.
		 */
		virtual void flush(Callback cb);
		/**
		 clear buffered output
		 */
		virtual void clear();
		/**
		 internal method do not use.
		 */
		void addDefaultHeaders();
		/**
		 Serialize the HTTP response headers.
		 */
		void serializeHeaders(CP::StreamWriter& sw);
		/**
		 Write data to the HTTP response body.
		 */
		template<class ... T>
		void write(T&&... a) {
			output.write(std::forward<T>(a)...);
		}
		/**
		 internal method do not use.
		 */
		void _writeCB(int r);
		/**
		 internal method do not use.
		 */
		virtual void reset();
	};
	class Page;
	/**
	 Represents a cppsp web server instance.
	 */
	class Server: public RGC::Object
	{
	public:
		Server();
		/**
		 The handle request callback. It is called for every request right after reading in all the request data and instantiating the Request and Response objects.
		 You can override the behavior of the web server by attaching (from within a cppsp module). See the example .cppsm modules in /www (included with the cppsp source).
		 */
		DelegateChain<void(Request&, Response&, Delegate<void()>)> handleRequest;
		/**
		 Load static file from "path" and serve it to the specified response. path is relative to server root.
		 */
		virtual void handleStaticRequest(String path, Request& req, Response& resp,
				Delegate<void()> cb)=0;
		/**
		 Load dynamic page from "path" and serve it to the specified response. path is relative to server root.
		 */
		virtual void handleDynamicRequest(String path, Request& req, Response& resp,
				Delegate<void()> cb)=0;
		/**
		 default request router.
		 */
		void defaultHandleRequest(Request& req, Response& resp, Delegate<void()> cb);
		/**
		 Returns absolute path of server root directory.
		 */
		virtual String rootDir()=0;
		/**
		 Load and instantiate dynamic page from "path". path is relative to server root.
		 */
		virtual void loadPage(CP::Poll& p, String path, RGC::Allocator& a,
				Delegate<void(Page*, exception* ex)> cb)=0;
		/**
		 Load and instantiate dynamic page from "path". path is absolute.
		 */
		virtual void loadPageFromFile(CP::Poll& p, String path, RGC::Allocator& a,
				Delegate<void(Page*, exception* ex)> cb)=0;

		/**
		 Load cppsp module from "path". path is relative to server root.
		 */
		virtual void loadModule(CP::Poll& p, String path, Delegate<void(void*, exception* ex)> cb)=0;
		/**
		 Load cppsp module from "path". path is absolute.
		 */
		virtual void loadModuleFromFile(CP::Poll& p, String path,
				Delegate<void(void*, exception* ex)> cb)=0;
		/**
		 Load static page from "path". path is relative to server root.
		 */
		virtual String loadStaticPage(String path)=0;
		/**
		 Load static page from "path". path is absolute.
		 */
		virtual String loadStaticPageFromFile(String path)=0;
		/**
		 Given a path relative to the server root, returns the absolute path.
		 */
		virtual string mapPath(string path);
		/**
		 Given a path relative to the server root, returns the absolute path. The returned string is allocated from "a".
		 */
		virtual String mapPath(String path, RGC::Allocator& a);
		/**
		 Returns the cppspManager instance for this Server instance; may be NULL if the underlying server does not use cppspManager.
		 */
		virtual cppspManager* manager() {
			return NULL;
		}
	};
	/**
	 Base class for all cppsp pages.
	 */
	class Page: public RGC::Object
	{
	public:
		typedef Delegate<void()> Callback;
		Page(Request& req, Response& resp, CP::StringPool* sp);
		/**
		 Object representing the HTTP request.
		 */
		RGC::Ref<Request> request;
		/**
		 Object representing the HTTP response.
		 */
		RGC::Ref<Response> response;
		/**
		 internal field do not use.
		 */
		Delegate<void(Page*, exception* ex)> pageCB;
		/**
		 Poll instance for the current thread. Can be used for asynchronous I/O, timers, etc.
		 */
		CP::Poll* poll;
		/**
		 The current Server instance. Usually there is one Server instance for every thread.
		 */
		Server* server;
		/**
		 internal field do not use.
		 */
		Callback cb;
		/**
		 Absolute path of the current cppsp page.
		 */
		String filePath;
		/**
		 The per-request StringPool instance. You can allocate memory from this if it only need to persist for the duration of this http request. The StringPool is cleared after the page has finished executing.
		 */
		CP::StringPool* sp;
		bool doRender;
		bool doReadPost;
		Page() :
				doRender(true) {
		}
		inline void cancelRender() {
			doRender = false;
		}
		/**
		 internal field do not use. Pointer to mmap()ed address of the string table file.
		 */
		const uint8_t* __stringTable;
		/**
		 internal method do not use.
		 */
		void __writeStringTable(int i, int len);
		/**
		 Given a path relative to the current page, returns the absolute path. The returned string is allocated from the request-wide StringPool and is invalid after the page finishes executing.
		 */
		String mapPath(String path);
		/**
		 Given a path relative to the current page, returns the absolute path. The returned string is allocated from "a".
		 */
		String mapPath(String path, RGC::Allocator& a);
		/**
		 Given a path relative to the current page, returns the path relative to the server root. The returned string is allocated from the request-wide StringPool and is invalid after the page finishes executing.
		 */
		String mapRelativePath(String path);
		/**
		 Given a path relative to the current page, returns the path relative to the server root. The returned string is allocated from "a".
		 */
		String mapRelativePath(String path, RGC::Allocator& a);
		/**
		 Load and instantiate dynamic page from "path". path is relative to the current page.
		 */
		void loadNestedPage(String path, Delegate<void(Page*, exception* ex)> cb);
		/**
		 Load and instantiate dynamic page from "path". path is relative to the current page. Returned Page object is allocated from "a".
		 */
		void loadNestedPage(String path, Delegate<void(Page*, exception* ex)> cb, RGC::Allocator& a);
		/**
		 Load and instantiate dynamic page from "path". path is absolute.
		 */
		void loadNestedPageFromFile(String path, Delegate<void(Page*, exception* ex)> cb);
		/**
		 Load and instantiate dynamic page from "path". path is absolute. Returned Page object is allocated from "a".
		 */
		void loadNestedPageFromFile(String path, Delegate<void(Page*, exception* ex)> cb,
				RGC::Allocator& a);
		/**
		 Called by the web server immediately after instantiating the current page. The page calls "cb" when it is done processing, to signal the web server that it can clean up the StringPool and all request-related data. The object pointed to by fields "request" and "response" may be recycled.
		 */
		virtual void handleRequest(Callback cb = nullptr);
		virtual void processRequest() {
			doInit();
		}
		/**
		 Each page goes through these stages (in order): init, [readPost], load, render, flush, finalize, [web server callback], ~Page

		 To be overridden by base class; called before load() is called, and must call initCB() or cancelLoad() after it's done
		 */
		virtual void init() {

		}
		virtual void doInit();
		virtual void initCB();
		virtual void cancelLoad(exception* ex = NULL);
		/**
		 To be overridden by derived class; called before headers are written. See init() for a description of the request life-cycle.
		 */
		virtual void load() {
		}
		virtual void render(CP::StreamWriter& out);
		/**
		 Called after rendering. See init() for a description of the request life-cycle.
		 */
		virtual void flush();
		/**
		 Called by flush() after it is done writing. See init() for a description of the request life-cycle.
		 */
		virtual void flushCB() {
			finalize();
		}
		/**
		 Called after flush() is done. See init() for a description of the request life-cycle.
		 */
		virtual void finalize() {
			finalizeCB();
		}
		/**
		 Called by finalize() after it is done doing its work. This will call the web server's callback and cause it to clean-up and re-cycle the Request and Response objects, and destroy the Page object. See init() for a description of the request life-cycle.
		 */
		virtual void finalizeCB();
		virtual ~Page() {
		}

		void _readPOSTCB(Request& r);
		void _flushCB(Response& r);
		void _pageCB(Page* p, exception* ex);
	};

} /* namespace cppsp */
#endif /* PAGE_H_ */
