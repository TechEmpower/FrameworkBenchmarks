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
 * httpparser.H
 *
 *  Created on: Apr 28, 2013
 *      Author: xaxaxa
 */

#ifndef HTTPPARSER_H_
#define HTTPPARSER_H_

#include <cpoll/cpoll.H>
#include "headercontainer.H"

#define CPPSP_MAXHEADERS 32
namespace cppsp
{
	using namespace CP;
	struct HTTPParser: public virtual RGC::Object
	{
		static inline char tolower(char c) {
			if (c <= 'Z' && c >= 'A') c = c - 'A' + 'a';
			return c;
		}
		//s2 must be all lowercase!!!!!
		static bool ci_equals(String s1, String s2) {
			if (s1.length() != s2.length()) return false;
			if (s1.length() == 0) return true;
			for (int i = 0; i < s1.length(); i++) {
				if (tolower(s1.data()[i]) != s2.data()[i]) return false;
			}
			return true;
		}

		headerContainer* hc;
		MemoryStream ms;
		String content;
		String reqLine;
		Delegate<bool()> state;
		int pos;
		int rpos;
		int _ctLen;
		int reqLine_i;
		int headerpos[CPPSP_MAXHEADERS];
		int headercount = 0;
		bool firstLine = true;
		HTTPParser(headerContainer* hc) :
				hc(hc), ms(8192), pos(0), rpos(0), _ctLen(0) {
			state= {&HTTPParser::_process_readingHeaders,this};
		}
		String beginPutData(int len) {
			if (ms.bufferSize - ms.bufferPos < len) ms.flushBuffer(len);
			return {(char*)ms.buffer + ms.bufferPos,ms.bufferSize-ms.bufferPos};
		}
		void endPutData(int len) {
			ms.bufferPos += len;
			ms.flush();
		}

		static inline char* findCRLF(const void* s, int len) {
			char* tmp = (char*) memchr(s, '\r', len);
			if (unlikely(tmp == NULL || tmp >= ((char*) s + len))) return NULL;
			if (tmp[1] == '\n') return tmp;
			else return (char*) memmem(tmp + 1, (char*) s + len - tmp - 1, "\r\n", 2);
		}
		bool _process_readingHeaders() {
			uint8_t* buf = ms.data();
			aaaaa: void* tmp = findCRLF(buf + rpos, ms.length() - rpos);
			if (tmp == NULL) {
				//minus one to catch any delimiters that might be cut off
				//in the middle
				if (rpos < ms.length() - 1) rpos = ms.length() - 1;
				return false;
			}
			int newpos = ((uint8_t*) tmp) - buf;
			//line: (buf+pos, newpos-pos)
			uint8_t* lineBuf = buf + pos;
			int lineBufLen = newpos - pos;

			if (firstLine) {
				reqLine.len = newpos - pos;
				reqLine_i = pos;
				firstLine = false;
			} else {
				if (lineBufLen == 0) {
					auto* items=hc->beginReplace(headercount);
					for(int i=0;i<headercount;i++) {
						int pos1=headerpos[i];
						int end=(i+1<headercount?headerpos[i+1]:this->pos)-2;
						lineBuf=buf+pos1;
						lineBufLen=end-pos1;
						tmp = memchr(lineBuf, ':', lineBufLen);
						if (tmp == NULL) {
							*(items++)= {(char*)lineBuf,(char*)NULL,lineBufLen,0};
						} else {
							headerContainer::item it;
							uint8_t* tmp1 = (uint8_t*) tmp - 1;
							while (tmp1 >= lineBuf && *tmp1 == ' ') tmp1--;
							it.name = (char*)lineBuf;
							it.nameLength = (int) (tmp1 - lineBuf + 1);
							tmp1 = (uint8_t*) tmp + 1;
							while (tmp1 < (buf + newpos) && *tmp1 == ' ') tmp1++;
							it.value = (char*)tmp1;
							it.valueLength = (int) (lineBuf + lineBufLen - tmp1);

							if (_ctLen == 0
									&& ci_equals( {(char*) lineBuf, it.nameLength}, "content-length")) {
								_ctLen = atoi( {(char*) tmp1, it.valueLength});
							}
							*(items++)=it;
						}
					}
					hc->endReplace();
					rpos = pos = newpos + 2;
					state = {&HTTPParser::_process_readingContent,this};
					return _process_readingContent();
				}
				if(headercount>=CPPSP_MAXHEADERS) goto skipheader;
				headerpos[headercount++]=pos;
				skipheader:;
			}
			rpos = pos = newpos + 2;
			goto aaaaa;
		}
		bool _process_readingContent() {
			uint8_t* buf = ms.data();
			if (ms.length() - pos < _ctLen) return false;
			content= {(char*)buf+pos,_ctLen};
			pos += _ctLen;
			rpos = pos;
			_ctLen = 0;
			state = {&HTTPParser::_process_readingHeaders,this};
			firstLine = true;
			headercount=0;
			reqLine.d = (char*) buf + reqLine_i;
			return true;
		}
		//returns whether or not a complete http request was found
		//headers will be added to *hc, and content will be set to point
		//to any content received
		//note: *hc may be modified even when it returns false
		inline bool process() {
			if (pos >= ms.length()) return false;
			return state();
		}
		inline String getBufferData() {
			return {(char*)ms.buffer + pos, ms.length() - pos};
		}
		inline String getHistory(bool includeUnprocessed = true) {
			return {(char*)ms.buffer, includeUnprocessed ? ms.length() : pos};
		}
		void skip(int length) {
			pos+=length;
			if(rpos<pos)rpos=pos;
		}

		//free up buffer space
		void reset() {
			if (pos > 0) {
				int shift=firstLine?pos:reqLine_i;
				if (ms.length() - shift > 0) memmove(ms.buffer, ms.buffer + shift, ms.length() - shift);
				ms.len -= shift;
				rpos -= shift;
				pos -= shift;
				ms.bufferPos = ms.len;
			}
		}
	};
	class HTTPStream: public CP::Stream
	{
	public:
		HTTPParser* parser;
		Stream* stream;
		int32_t tryFixRead(void* buf, int32_t len) {
			int& bufPos = parser->pos;
			int bufLen = parser->ms.length();
			if (bufPos >= bufLen) return -1;
			int32_t l = len > (bufLen - bufPos) ? (bufLen - bufPos) : len;
			if (l <= 0) return 0;
			memcpy(buf, parser->ms.buffer + bufPos, l);
			bufPos += l;
			if (parser->rpos < bufPos) parser->rpos = bufPos;
			return l;
		}
		int32_t read(void* buf, int32_t len) {
			int32_t r;
			if ((r = tryFixRead(buf, len)) == -1) return stream->read(buf, len);
			else return r;
		}
		int32_t write(const void* buf, int32_t len) {
			return stream->write(buf, len);
		}
		void read(void* buf, int32_t len, const Callback& cb, bool repeat = false) {
			int32_t r;
			if ((r = tryFixRead(buf, len)) == -1) stream->read(buf, len, cb, repeat);
			else {
				cb(r);
				if (repeat && r > 0) stream->read(buf, len, cb, true);
			}
		}
		void write(const void* buf, int32_t len, const Callback& cb, bool repeat = false) override {
			stream->write(buf, len, cb, repeat);
		}
		//sync
		void close() override {
		}
		void flush() override {
			stream->flush();
		}

		//async
		void close(const Callback& cb) override {
		}
		void flush(const Callback& cb) override {
			stream->flush(cb);
		}
		void cancelRead() override {
			stream->cancelRead();
		}
		void cancelWrite() override {
			stream->cancelWrite();
		}
		int32_t readBuffer(void*& buf, int32_t maxlen) override {
			int& bufPos = parser->pos;
			int bufLen = parser->ms.length();
			if (bufPos >= bufLen) return 0;
			if (bufLen - bufPos < maxlen) maxlen = bufLen - bufPos;
			buf = parser->ms.buffer + bufPos;
			return maxlen;
		}
		void freeBuffer(void* buf, int32_t len) override {
			int& bufPos = parser->pos;
			bufPos += len;
			if (parser->rpos < bufPos) parser->rpos = bufPos;
		}
	};
}

#endif /* HTTPPARSER_H_ */
