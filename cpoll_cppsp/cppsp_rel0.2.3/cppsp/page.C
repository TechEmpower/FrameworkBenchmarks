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
 * page.C
 *
 *  Created on: Jan 26, 2013
 *      Author: xaxaxa
 */

#include "include/page.H"
#include "include/common.H"
#include <stdexcept>
#include <stdlib.h>
#include <math.h>

using namespace CP;
namespace cppsp
{
	static inline int itoa(int i, char* b) {
		static char const digit[] = "0123456789";
		char* p = b;
		//negative detection is not needed for this specific use-case
		//(writing the content-length header)
		/*if (i < 0) {
		 *p++ = '-';
		 i = -i;
		 }*/
		p += (i==0?0:int(log10f(i))) + 1;
		*p = '\0';
		int l = p - b;
		do { //Move back, inserting digits as u go
			*--p = digit[i % 10];
			i = i / 10;
		} while (i);
		return l;
	}
	//inline-able memcpy() for copying SHORT STRINGS ONLY
	static inline void memcpy2_1(void* dst, const void* src, int len) {
		for (int i = 0; i < len; i++)
			((char*) dst)[i] = ((const char*) src)[i];
	}
	Page::Page(Request& req, Response& resp, CP::StringPool* sp) :
			request(&req), response(&resp), sp(sp), doRender(true) {
	}
	void Page::__writeStringTable(int i, int len) {
		response->output.write(__stringTable + i, len);
	}
	void Page::handleRequest(Callback cb) {
		this->cb = cb;
		try {
			processRequest();
		} catch (exception& ex) {
			handleError(&ex, *response, this->filePath);
			flush();
		}
	}
	void Page::render(CP::StreamWriter& out) {
		out.write("This is the default page of the cppsp C++ "
				"web application framework. If you see this, it means "
				"you haven't overridden the render() method derived from cppsp::Page.");
	}
	void Page::doInit() {
		doReadPost = false;
		init();
		if (doReadPost && request->method.compare("POST") == 0) request->readPost( {
				&Page::_readPOSTCB, this });
		else initCB();
	}
	void Page::initCB() {
		try {
			load();
			//response->writeHeaders();
			if (doRender) render(response->output);
			flush();
		} catch (exception& ex) {
			handleError(&ex, *response, this->filePath);
			flush();
		}
	}
	void Page::cancelLoad(exception* ex) {
		if (ex == NULL) {
			runtime_error re("Web page execution cancelled");
			handleError(&re, *response, this->filePath);
		} else handleError(ex, *response, this->filePath);
		response->flush( { &Page::_flushCB, this });
	}
	void Page::_readPOSTCB(Request& r) {
		initCB();
	}
	void Page::flush() {
		if (response->closed) finalizeCB();
		else response->flush( { &Page::_flushCB, this });
	}
	String Page::mapPath(String path) {
		return server->mapPath(mapRelativePath(path), *sp);
	}
	String Page::mapPath(String path, RGC::Allocator& a) {
		return server->mapPath(mapRelativePath(path, a), a);
	}
	String Page::mapRelativePath(String path) {
		return mapRelativePath(path, *sp);
	}
	String Page::mapRelativePath(String path, RGC::Allocator& a) {
		char *tmp = (char*) a.alloc(request->path.length() + path.length());
		int l = combinePath(request->path.data(), request->path.length(), path.data(), path.length(),
				tmp);
		return {tmp,l};
	}
	void Page::loadNestedPage(String path, Delegate<void(Page*, exception* ex)> cb) {
		loadNestedPage(path, cb, *sp);
	}
	void Page::loadNestedPage(String path, Delegate<void(Page*, exception* ex)> cb,
			RGC::Allocator& a) {
		this->pageCB = cb;
		String tmp = mapRelativePath(path, a);
		server->loadPage(*poll, { tmp.data(), (int) tmp.length() }, a, { &Page::_pageCB, this });
	}
	void Page::loadNestedPageFromFile(String path, Delegate<void(Page*, exception* ex)> cb) {
		loadNestedPageFromFile(path, cb, *sp);
	}
	void Page::loadNestedPageFromFile(String path, Delegate<void(Page*, exception* ex)> cb,
			RGC::Allocator& a) {
		this->pageCB = cb;
		server->loadPageFromFile(*poll, { path.data(), (int) path.length() }, a, { &Page::_pageCB,
				this });
	}
	
	void Page::_pageCB(Page* p, exception* ex) {
		if (p != NULL) {
			p->request = request;
			p->response = response;
			p->poll = poll;
			p->server = server;
		}
		pageCB(p, ex);
	}
	void Page::_flushCB(Response& r) {
		flushCB();
	}
	void Page::finalizeCB() {
		if (this->cb != nullptr) cb();
	}

	Request::Request(CP::Stream& inp, CP::StringPool* sp) :
			inputStream(&inp), sp(sp), alloc(sp), headers(sp), queryString(less<String>(), alloc),
					form(less<String>(), alloc) {
	}
	void Request::parsePost(String buf) {
		struct
		{
			Request* This;
			void operator()(const char* name, int nameLen, const char* value, int valueLen) {
				String n, v;
				n = cppsp::urlDecode(name, nameLen, *This->sp);
				if (value != NULL) {
					v = cppsp::urlDecode(value, valueLen, *This->sp);
				} else v= {(char*)nullptr,0};
				This->form[n] = v;
			}
		} cb { this };
		cppsp::parseQueryString(buf.data(), buf.length(), &cb, false);
	}

	Response::Response(CP::Stream& out, CP::StringPool* sp) :
			outputStream(&out), output((CP::BufferedOutput&) buffer), sp(sp), alloc(sp),
					headers(less<String>(), alloc), headersWritten(false), closed(false),
					sendChunked(false) {
		addDefaultHeaders();
	}
	void Response::addDefaultHeaders() {
		statusCode = 200;
		statusName = "OK";
		headers.insert( { "Content-Type", "text/html; charset=UTF-8" });
	}
	void Response_doWriteHeaders(Response* This, CP::StreamWriter& sw) {
		//sw.writeF("HTTP/1.1 %i %s\r\n", This->statusCode, This->statusName);
		{
			char* s1 = sw.beginWrite(32);
			memcpy2_1(s1, "HTTP/1.1 ", 9);
			int x = 9 + itoa(This->statusCode, s1 + 9);
			s1[x] = ' ';
			x++;
			memcpy2_1(s1 + x, This->statusName.data(), This->statusName.length());
			x += This->statusName.length();
			s1[x] = '\r';
			s1[x + 1] = '\n';
			x += 2;
			sw.endWrite(x);
		}
		for (auto it = This->headers.begin(); it != This->headers.end(); it++) {
			int l1 = (*it).first.length();
			int l2 = (*it).second.length();
			char* tmp = sw.beginWrite(l1 + 4 + l2);
			memcpy2_1(tmp, (*it).first.data(), l1);
			tmp[l1] = ':';
			tmp[l1 + 1] = ' ';
			memcpy2_1(tmp + l1 + 2, (*it).second.data(), l2);
			tmp[l1 + 2 + l2] = '\r';
			tmp[l1 + 2 + l2 + 1] = '\n';
			sw.endWrite(l1 + 4 + l2);
			//sw.writeF("%s: %s\r\n", (*it).first.c_str(), (*it).second.c_str());
		}
		sw.write("\r\n", 2);
	}
	void Response::serializeHeaders(CP::StreamWriter& sw) {
		Response_doWriteHeaders(this, sw);
	}
	void Response::flush(Callback cb) {
		//printf("flush\n");
		if (closed) throw runtime_error("connection has already been closed by the client");
		output.flush();
		this->_cb = cb;
		if (likely(!headersWritten)) {
			headersWritten = true;
			int bufferL = buffer.length();
			{
				char* tmps = sp->beginAdd(16);
				int l = itoa(buffer.length(), tmps);
				sp->endAdd(l);
				this->headers.insert( { "Content-Length", { tmps, l } });
				CP::StreamWriter sw(buffer);
				Response_doWriteHeaders(this, sw);
			}
			iov[0]= {buffer.data()+bufferL, (size_t)(buffer.length()-bufferL)};
			iov[1]= {buffer.data(), (size_t)bufferL};
			outputStream->writevAll(iov, 2, { &Response::_writeCB, this });
			return;
		} else {
			if (buffer.length() <= 0) {
				cb(*this);
				return;
			}
			outputStream->write(buffer.data(), buffer.length(), { &Response::_writeCB, this });
		}
	}
	void Response::clear() {
		output.flush();
		buffer.clear();
		headersWritten = false;
	}
	void Response::_writeCB(int r) {
		if (r <= 0) closed = true;
		buffer.clear();
		_cb(*this);
	}
	
	void Request::reset() {
		headers.clear();
		queryString.clear();
		form.clear();
	}
	void Response::reset() {
		headers.clear();
		addDefaultHeaders();
		output.flush();
		buffer.clear();
		headersWritten = false;
		closed = false;
		sendChunked = false;
	}

	Server::Server() {
		handleRequest.attach( { &Server::defaultHandleRequest, this });
	}
	void Server::defaultHandleRequest(Request& req, Response& resp, Delegate<void()> cb) {
		if (req.path.length() > 6
				&& memcmp(req.path.data() + (req.path.length() - 6), ".cppsp", 6) == 0) handleDynamicRequest(
				req.path, req, resp, cb);
		else handleStaticRequest(req.path, req, resp, cb);
	}
	String Server::mapPath(String path, RGC::Allocator& a) {
		String r = rootDir();
		char* tmp = (char*) a.alloc(path.length() + r.length());
		int l = cppsp::combinePathChroot(r.data(), r.length(), path.data(), path.length(), tmp);
		return String(tmp, l);
	}

} /* namespace cppsp */

string cppsp::Server::mapPath(string path) {
	String r = rootDir();
	char tmp[path.length() + r.length()];
	int l = cppsp::combinePathChroot(r.data(), r.length(), path.data(), path.length(), tmp);
	return string(tmp, l);
}
