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
 * cppsp_socketd.C
 *
 *  Created on: Mar 8, 2013
 *      Author: xaxaxa
 */
#include "include/cppsp_cpoll.H"
#include <stdio.h>
#include <unistd.h>
#include "include/stringutils.H"
using namespace CP;
namespace cppsp
{
	//static int CPollRequest::bufSize=4096;
	CPollRequest::CPollRequest(CP::Socket& s, CP::StringPool* sp) :
			Request(s, sp), _parser(&headers), s(s) {
		_stream.parser = &_parser;
		_stream.stream = &s;
		_stream.stream->retain();
		this->inputStream = &_stream;
	}
	bool CPollRequest_parseReqLine(CPollRequest* This) {
		uint8_t* lineBuf = (uint8_t*) This->_parser.reqLine.data();
		int lineBufLen = This->_parser.reqLine.length();
		uint8_t* tmp = (uint8_t*) memchr(lineBuf, ' ', lineBufLen);
		if (tmp == NULL) return false;
		This->method = {(char*) lineBuf, int(tmp - lineBuf)};
		tmp++;
		if (lineBuf + lineBufLen - tmp <= 0) return false;
		uint8_t* tmp1 = (uint8_t*) memchr(tmp, ' ', lineBuf + lineBufLen - tmp);
		if (tmp1 == NULL) return false;
		const char* path = (const char*) tmp;
		int pathLen = tmp1 - tmp;
		if (pathLen <= 0) return false;

		const char* q = (const char*) memchr(path, '?', pathLen);
		if (q == NULL) This->path = {path, pathLen};
		else {
			struct
			{
				CPollRequest* This;
				void operator()(const char* name, int nameLen, const char* value, int valueLen) {
					String n, v;
					n=cppsp::urlDecode(name, nameLen, *This->sp);
					if (value != NULL) {
						v=cppsp::urlDecode(value, valueLen, *This->sp);
					} else v= {(char*)nullptr,0};
					This->queryString[n] = v;
				}
			}cb {This};
			cppsp::parseQueryString(q + 1, path + pathLen - q - 1, &cb, false);
			This->path = {path, q - path};
		}
		return true;
	}
	bool CPollRequest::readRequest(const Delegate<void(bool)>& cb) {
		if (_parser.process()) {
			if (CPollRequest_parseReqLine(this)) return true;
			else {
				cb(false);
				return false;
			}
		} else {
			this->tmp_cb = cb;
			_parser.reset();
			_beginRead();
			return false;
		}
	}
	void CPollRequest::_beginRead() {
		String b = _parser.beginPutData(4096);
		_stream.stream->read(b.data(), b.length(), { &CPollRequest::_readCB, this });
	}
	void CPollRequest::_readCB(int i) {
		if (i <= 0) {
			tmp_cb(false);
			return;
		}
		_parser.endPutData(i);
		if (_parser.process()) {
			tmp_cb(CPollRequest_parseReqLine(this));
		} else {
			_beginRead();
		}
	}
	CPollRequest::~CPollRequest() {
		_stream.stream->release();
	}
}
