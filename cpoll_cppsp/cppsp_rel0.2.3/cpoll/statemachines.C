/*
 * statemachines.C
 *
 *  Created on: Jan 31, 2013
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
//state machines for asynchronously processing input streams
#include <tuple>
#include <functional>
#include <stdint.h>
#include <tuple>
#include <string>
#include <string.h>
#include "include/statemachines.H"
#include <stdio.h>
using namespace std;
namespace CP
{
	void* memcpymove(uint8_t* dst, const uint8_t* src, size_t n) {
		if ((dst >= src + n) || (src >= dst + n)) {
			//not overlapping
			return memcpy(dst, src, n);
		} else return memmove(dst, src, n);
	}
	struct streamReader
	{
		//you must allocate memory for this struct such that there is (bufferSize) bytes of allocated space
		//right after this struct.
		//ex.
		//streamReader* sr=(streamReader*)malloc(sizeof(streamReader)+4096);
		//new (sr) streamReader(4096);

		//user specified
		//				data	len	delimReached
		Delegate<void(uint8_t*, int, bool)> output;
		void* buffer;
		int bufferCapacity;

		//internal
		int bufferLen;
		int bufferPos;
		int state; //0: none; 1: readUntilString; 2: readUntilChar
		const char* delim1;
		int delim1len;
		char delim2;
		//bool delayProcessing;
		//bool delayedProcessing;
		inline void reset() {
			bufferPos = 0;
			bufferLen = 0;
			state = 0;
			//delayProcessing = false;
		}
		streamReader(void* buffer, int capacity) {
			this->buffer = buffer;
			bufferCapacity = capacity;
			reset();
		}
		streamReader(int capacity) {
			this->buffer = (void*) (this + 1);
			bufferCapacity = capacity;
			reset();
		}
		inline uint8_t* getBuffer() {
			return (uint8_t*) buffer;
		}
		inline void setCallback(const Delegate<void(uint8_t*, int, bool)>& cb) {
			output = cb;
		}

		void process() {
			/*if (delayProcessing) {
			 delayedProcessing = true;
			 return;
			 }*/
			if (bufferPos >= bufferLen) {
				bufferPos = bufferLen = 0;
				return;
			}
			switch (state) {
				case 0:
					break;
				case 1:
				{
					uint8_t* buf = getBuffer();
					if (bufferLen - bufferPos < (int) delim1len && bufferPos > 0) {
						asdfg: memmove(buf, buf + bufferPos, bufferLen - bufferPos);
						bufferLen -= bufferPos;
						bufferPos = 0;
						return;
					}

					//printf("%i\n",bufferLen - bufferPos);
					uint8_t* tmp = (uint8_t*) memmem(buf + bufferPos, bufferLen - bufferPos, delim1,
							delim1len);
					if (tmp == NULL) {
						//delayProcessing = true;
						//if (bufferLen - bufferPos - delim1len + 1 <= 0) break;
						output(buf + bufferPos, bufferLen - bufferPos - delim1len + 1, false);
						//delayProcessing = false;
						//memmove(buf, buf + bufferLen - delim1.length(), delim1.length());
						bufferPos = bufferLen - delim1len + 1;
						goto asdfg;
					} else {
						int oldPos = bufferPos;
						int newPos = tmp - buf;
						bufferPos = newPos + delim1len;
						if (bufferPos >= bufferLen) {
							bufferLen = bufferPos = 0;
						}
						state = 0;
						output(buf + oldPos, newPos - oldPos, true);
					}
					break;
				}
				case 2:
				{
					uint8_t* buf = getBuffer();
					uint8_t* tmp = (uint8_t*) memchr(buf + bufferPos, delim2, bufferLen - bufferPos);
					int oldPos = bufferPos;
					if (tmp == NULL) {
						int oldLen = bufferLen;
						bufferLen = bufferPos = 0;
						output(buf + oldPos, oldLen - oldPos, false);
					} else {
						int newPos = tmp - buf;
						bufferPos = newPos + 1;
						if (bufferPos >= bufferLen) {
							bufferLen = bufferPos = 0;
						}
						state = 0;
						output(buf + oldPos, newPos - oldPos, true);
					}
					break;
				}
			}
			//if (delayedProcessing) goto reprocess;
		}
		inline void readUntilString(const char* delim, int delimLen) {
			state = 1;
			delim1 = delim;
			delim1len = delimLen;
			//printf("%i\n",delim.length());
			process();
		}
		inline void readUntilChar(char delim) {
			state = 2;
			delim2 = delim;
			process();
		}
		//		buf		length
		inline tuple<uint8_t*, int> beginPutData() {
			//printf("%i %i\n",bufferLen, bufferCapacity - bufferLen);
			return make_tuple(getBuffer() + bufferLen, bufferCapacity - bufferLen);
		}
		//len <= length returned from beginPutData()
		inline void endPutData(int len) {
			//printf("%i\n",len);
			bufferLen += len;
			if (len > 0) process();
		}
		inline tuple<uint8_t*, int> getBufferData() {
			return make_tuple(getBuffer() + bufferPos, bufferLen - bufferPos);
		}
		inline void skip(int i) {
			bufferPos += i;
		}
	};
	int streamReader_getSize() {
		return sizeof(streamReader);
	}
	void streamReader_init(streamReader* sr, int capacity) {
		new (sr) streamReader(capacity);
	}
	void streamReader_init(streamReader* sr, void* buffer, int capacity) {
		new (sr) streamReader(buffer, capacity);
	}
	void streamReader_deinit(streamReader* sr) {
		sr->~streamReader();
	}
	tuple<uint8_t*, int> streamReader_beginPutData(streamReader* sr) {
		return sr->beginPutData();
	}
	void streamReader_endPutData(streamReader* sr, int len) {
		sr->endPutData(len);
	}
	void streamReader_readUntilString(streamReader* sr, const char* delim, int delimLen) {
		sr->readUntilString(delim, delimLen);
	}
	void streamReader_readUntilChar(streamReader* sr, char delim) {
		sr->readUntilChar(delim);
	}
	void streamReader_setCallback(streamReader* sr, const Delegate<void(uint8_t*, int, bool)>& cb) {
		sr->setCallback(cb);
	}
	tuple<uint8_t*, int> streamReader_getBufferData(streamReader* sr) {
		return sr->getBufferData();
	}
	void streamReader_reset(streamReader* sr) {
		sr->reset();
	}
	void streamReader_skip(streamReader* sr, int i) {
		sr->skip(i);
	}
}

