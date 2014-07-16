/*
 * statemachines.H
 *
 *  Created on: Feb 1, 2013
 *      Author: xaxaxa
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
#ifndef STATEMACHINES_H_
#define STATEMACHINES_H_
#include <functional>
#include <stdint.h>
#include <string>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <stdexcept>
#include <delegate.H>
#include "basictypes.H"
#ifndef likely
#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)
#endif

using namespace std;
namespace CP
{
	//streamReader C-style API/ABI; this is to avoid breaking the ABI when the
	//internal state machine structure changes
	//usage example:
	//streamReader* sr=(streamReader*)malloc(streamReader_getSize()+4096);
	//streamReader_init(sr, 4096);
	// ...
	//streamReader_deinit(sr);

	//it is safe to delete a streamReader from within your callback
	struct streamReader;
	int streamReader_getSize();
	void streamReader_init(streamReader* sr, void* buffer, int capacity);
	void streamReader_init(streamReader* sr, int capacity);
	void streamReader_deinit(streamReader* sr);
	tuple<uint8_t*, int> streamReader_beginPutData(streamReader* sr);
	void streamReader_endPutData(streamReader* sr, int len);
	void streamReader_readUntilString(streamReader* sr, const char* delim, int delimLen);
	void streamReader_readUntilChar(streamReader* sr, char delim);
	void streamReader_setCallback(streamReader* sr, const Delegate<void(uint8_t*, int, bool)>& cb);
	tuple<uint8_t*, int> streamReader_getBufferData(streamReader* sr);
	void streamReader_reset(streamReader* sr);
	void streamReader_skip(streamReader* sr, int i);

	//another streamReader implementation that remembers all data that has ever been fed into it;
	//mainly for socketd
	struct persistentStreamReader
	{
		uint8_t* buffer;
		int capacity;
		int len;
		int pos;
		int searchPos;
		int state; //0: none; 1: readUntilString; 2: readUntilChar
		//function<void(uint8_t*, int)> output;
		Delegate<void(uint8_t*, int)> output;
		const char* delim1;
		int delim1_len;
		char delim2;
		inline void clearBuffer() {
			searchPos = pos = len;
			//delayProcessing = false;
		}
		void resize(int nc) {
			uint8_t* tmp = (uint8_t*) realloc(buffer, nc);
			if (tmp == NULL) throw runtime_error(strerror(errno));
			buffer = tmp;
			capacity = nc;
		}
		void ensureCapacity(int c) {
			if (likely(c < capacity)) return;
			int nc = capacity;
			while (nc < c)
				nc *= 2;
			if (nc != capacity) resize(nc);
		}
		persistentStreamReader(int c = 4096) :
				capacity(c), len(0), pos(0), searchPos(0), state(0) {
			buffer = (uint8_t*) malloc(c);
			if (buffer == NULL) throw runtime_error(strerror(errno));
		}
		~persistentStreamReader() {
			free(buffer);
		}
		void process() {
			if (len <= searchPos) return;
			switch (state) {
				case 0:
					break;
				case 1:
				{
					uint8_t* tmp = (uint8_t*) memmem(buffer + searchPos, len - searchPos, delim1,
							delim1_len);
					if (tmp == NULL) {
						//overlap the search so that delimitors that are cut in half at
						//the end of the buffer can be caught
						searchPos = len - delim1_len;
					} else {
						int oldPos = pos;
						pos = searchPos = (tmp - buffer) + delim1_len;
						state = 0;
						output(buffer + oldPos, tmp - buffer - oldPos);
					}
					break;
				}
				case 2:
				{
					uint8_t* tmp = (uint8_t*) memchr(buffer + searchPos, delim2, len - searchPos);
					if (tmp == NULL) {
						searchPos = len;
					} else {
						int oldPos = pos;
						pos = searchPos = (tmp - buffer) + 1;
						state = 0;
						output(buffer + oldPos, tmp - buffer - oldPos);
					}
					break;
				}
			}
		}
		void readUntilString(const char* delim, int len) {
			state = 1;
			delim1 = delim;
			delim1_len = len;
			//printf("%i\n",delim.length());
			process();
		}
		void readUntilChar(char delim) {
			state = 2;
			delim2 = delim;
			process();
		}
		uint8_t* beginPutData(int len) {
			ensureCapacity(this->len + len);
			return buffer + this->len;
		}
		void endPutData(int len) {
			this->len += len;
			if (len > 0) process();
		}
		inline tuple<uint8_t*, int> getBufferData() {
			return make_tuple(buffer + pos, len - pos);
		}
		inline tuple<uint8_t*, int> getHistory(bool includeUnprocessed = true) {
			return make_tuple(buffer, includeUnprocessed ? len : pos);
		}
	};

	//revised version of streamReader which does not use callbacks
	struct newStreamReader
	{
		struct item
		{
			String data;
			bool delimReached;
		};
		uint8_t* buffer;
		const char* delim1;
		int bufferCapacity;
		int bufferLen;
		int bufferPos;
		int state; //0: none; 1: readUntilString; 2: readUntilChar
		int delim1len;
		char delim2;
		bool repeat;
		inline void reset() {
			bufferPos = 0;
			bufferLen = 0;
			state = 0;
			repeat = false;
			//delayProcessing = false;
		}
		newStreamReader(void* buffer, int capacity) {
			this->buffer = (uint8_t*) buffer;
			bufferCapacity = capacity;
			reset();
		}
		newStreamReader(int capacity) {
			this->buffer = (uint8_t*) (this + 1);
			bufferCapacity = capacity;
			reset();
		}
		inline void readUntilString(const char* delim, int delimLen, bool repeat = false) {
			state = 1;
			delim1 = delim;
			delim1len = delimLen;
			this->repeat = repeat;
		}
		inline void readUntilChar(char delim, bool repeat = false) {
			state = 2;
			delim2 = delim;
			this->repeat = repeat;
		}
		inline String beginPutData() {
			//printf("%i %i\n",bufferLen, bufferCapacity - bufferLen);
			return {(char*) buffer + bufferLen, bufferCapacity - bufferLen};
		}
		//len <= length returned from beginPutData()
		inline void endPutData(int len) {
			bufferLen += len;
		}
		inline String getBufferData() {
			return {(char*)buffer + bufferPos, bufferLen - bufferPos};
		}
		inline void skip(int i) {
			bufferPos += i;
		}
		bool process(item& it) {
			if (bufferPos >= bufferLen) {
				bufferPos = bufferLen = 0;
				return false;
			}
			bool ret = false;
			switch (state) {
				case 0:
					return false;
				case 1:
				{
					uint8_t* buf = buffer;
					if (bufferLen - bufferPos < (int) delim1len) {
						if (unlikely(bufferPos==0)) return false;
						asdfg: memmove(buf, buf + bufferPos, bufferLen - bufferPos);
						bufferLen -= bufferPos;
						bufferPos = 0;
						return ret;
					}
					uint8_t* tmp = (uint8_t*) memmem(buf + bufferPos, bufferLen - bufferPos, delim1,
							delim1len);
					if (tmp == NULL) {
						it= { {(char*)buf + bufferPos, bufferLen - bufferPos - delim1len + 1}, false};
						bufferPos = bufferLen - delim1len + 1;
						ret=true;
						goto asdfg;
					} else {
						int oldPos = bufferPos;
						int newPos = tmp - buf;
						bufferPos = newPos + delim1len;
						if (bufferPos >= bufferLen) {
							bufferLen = bufferPos = 0;
						}
						if(!repeat)state = 0;
						it= { {(char*)buf + oldPos, newPos - oldPos}, true};
						return true;
					}
				}
				case 2:
				{
					uint8_t* buf = buffer;
					uint8_t* tmp = (uint8_t*) memchr(buf + bufferPos, delim2, bufferLen - bufferPos);
					int oldPos = bufferPos;
					if (tmp == NULL) {
						int oldLen = bufferLen;
						bufferLen = bufferPos = 0;
						it= { {(char*)buf + oldPos, oldLen - oldPos}, false};
						return true;
					} else {
						int newPos = tmp - buf;
						bufferPos = newPos + 1;
						if (bufferPos >= bufferLen) {
							bufferLen = bufferPos = 0;
						}
						if (!repeat) state = 0;
						it= { {(char*)buf + oldPos, newPos - oldPos}, true};
						return true;
					}
				}
				default: return false;
			}
			//if (delayedProcessing) goto reprocess;
		}
	};
	struct newPersistentStreamReader
	{
		struct item
		{
			String data;
		};
		uint8_t* buffer;
		const char* delim1;
		int capacity;
		int len;
		int pos;
		int searchPos;
		int state; //0: none; 1: readUntilString; 2: readUntilChar
		int delim1_len;
		char delim2;
		bool repeat;
		inline void clearBuffer() {
			searchPos = pos = len;
		}
		void resize(int nc) {
			uint8_t* tmp = (uint8_t*) realloc(buffer, nc);
			if (tmp == NULL) throw runtime_error(strerror(errno));
			buffer = tmp;
			capacity = nc;
		}
		void ensureCapacity(int c) {
			if (likely(c < capacity)) return;
			int nc = capacity;
			while (nc < c)
				nc *= 2;
			if (nc != capacity) resize(nc);
		}
		void reset() {
			len = pos = searchPos = state = 0;
		}
		newPersistentStreamReader(int c = 4096) :
				capacity(c), len(0), pos(0), searchPos(0), state(0) {
			buffer = (uint8_t*) malloc(c);
			if (buffer == NULL) throw runtime_error(strerror(errno));
		}
		~newPersistentStreamReader() {
			free(buffer);
		}
		bool process(item& it) {
			if (len <= searchPos) return false;
			switch (state) {
				case 0:
					return false;
				case 1:
				{
					uint8_t* tmp = (uint8_t*) memmem(buffer + searchPos, len - searchPos, delim1,
							delim1_len);
					if (tmp == NULL) {
						//overlap the search so that delimitors that are cut in half at
						//the end of the buffer can be caught
						searchPos = len - delim1_len;
						return false;
					} else {
						int oldPos = pos;
						pos = searchPos = (tmp - buffer) + delim1_len;
						if (!repeat) state = 0;
						it= { {(char*)buffer + oldPos, int(tmp - buffer - oldPos)}};
						return true;
					}
				}
				case 2:
				{
					uint8_t* tmp = (uint8_t*) memchr(buffer + searchPos, delim2, len - searchPos);
					if (tmp == NULL) {
						searchPos = len;
						return false;
					} else {
						int oldPos = pos;
						pos = searchPos = (tmp - buffer) + 1;
						if (!repeat) state = 0;
						it= { {(char*)buffer + oldPos, int(tmp - buffer - oldPos)}};
						return true;
					}
				}
				default:
					return false;
			}
		}
		void readUntilString(const char* delim, int len, bool repeat = false) {
			state = 1;
			delim1 = delim;
			delim1_len = len;
			this->repeat = repeat;
		}
		void readUntilChar(char delim, bool repeat = false) {
			state = 2;
			delim2 = delim;
			this->repeat = repeat;
		}
		String beginPutData(int len) {
			ensureCapacity(this->len + len);
			return {(char*)buffer + this->len,this->capacity-this->len};
		}
		void endPutData(int len) {
			this->len += len;
		}
		inline String getBufferData() {
			return {(char*)buffer + pos, len - pos};
		}
		inline String getHistory(bool includeUnprocessed = true) {
			return {(char*)buffer, includeUnprocessed ? len : pos};
		}
		void clearHistory() {
			if (pos > 0) {
				memmove(buffer, buffer + pos, len - pos);
				len -= pos;
				searchPos -= pos;
				pos = 0;
			}
		}
	}
	;
}
#endif /* STATEMACHINES_H_ */
