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
 * basictypes.H
 *
 *  Created on: Apr 26, 2013
 *      Author: xaxaxa
 */

#ifndef BASICTYPES_H_
#define BASICTYPES_H_
#include <string>
#include <stdexcept>
#include <rgc.H>
using namespace std;
namespace CP
{
	typedef int32_t HANDLE; //file descriptor or handle
	typedef Delegate<void(int32_t)> Callback;
	typedef uint8_t event_t;
	struct String;
	class MemoryBuffer: virtual public RGC::Object
	{
	public:
		virtual uint8_t* data() const=0;
		virtual int length() const=0;
		inline CP::String String();
	};
	struct String
	{
		char* d;
		int len;
		String() :
				d(nullptr), len(0) {
		}
		String(std::nullptr_t) :
				d(nullptr), len(0) {
		}
		String(char* data, int length) :
				d(data), len(length) {
		}
		String(char* data) :
				d(data), len(data == nullptr ? 0 : strlen(data)) {
		}
		String(const char* data, int length) :
				d(const_cast<char*>(data)), len(length) {
		}
		String(const void* data, int length) :
				d((char*) (data)), len(length) {
		}
		String(const char* data) :
				d(const_cast<char*>(data)), len(strlen(data)) {
		}
		String(std::string s) :
				d((char*) s.data()), len(s.length()) {
		}
		String(MemoryBuffer& mb) :
				d((char*) mb.data()), len(mb.length()) {
		}
		int compare(const String& other) const {
			if (len < other.len) return -1;
			else if (len > other.len) return 1;
			else if (len == 0) return 0;
			else return memcmp(d, other.d, len);
		}
		inline bool operator<(const String& other) const {
			return compare(other) < 0;
		}
		inline bool operator>(const String& other) const {
			return compare(other) > 0;
		}
		inline bool operator<=(const String& other) const {
			return compare(other) <= 0;
		}
		inline bool operator>=(const String& other) const {
			return compare(other) >= 0;
		}
		inline bool operator==(const String& other) const {
			return compare(other) == 0;
		}
		inline bool operator==(std::nullptr_t other) const {
			return len == 0;
		}
		inline char* data() const {
			return d;
		}
		inline int length() const {
			return len;
		}
		inline std::string toSTDString() const {
			return std::string(d, len);
		}
		inline String subString(int i, int l) {
			if (i < 0 || i + l > this->len || l < 0) throw range_error(
					"String::subString() out of range");
			return String(d + i, l);
		}
		inline String subString(int i) {
			return subString(i, len - i);
		}
		inline void clip(int index, int length) {
			if (index < 0 || index + length > this->len || length < 0) throw range_error(
					"String::clip() out of range");
			d += index;
			len = length;
		}
		inline void clip(int index) {
			if (index < 0 || index > this->len) throw range_error("String::clip() out of range");
			d += index;
			len -= index;
		}
		inline int indexOf(String s, int i = 0) {
			void* tmp = memmem(d + i, len - i, s.d, s.len);
			return tmp == NULL ? -1 : (((char*) tmp) - d);
		}
		inline int indexOf(char c, int i = 0) {
			void* tmp = memchr(d + i, c, len - i);
			return tmp == NULL ? -1 : (((char*) tmp) - d);
		}
		inline char& operator[](int i) {
			return d[i];
		}
	};
	template<class ... T>
	String concat(RGC::Allocator& alloc, T ... s...) {
		int l=sizeof...(s);
		String tmp[sizeof...(s)] {s...};
		int size=0;
		for(int i=0;i<l;i++) size+=tmp[i].len;
		String r {(char*)alloc.alloc(size),0};
		for(int i=0;i<l;i++) {
			memcpy(r.d+r.len,tmp[i].d,tmp[i].len);
			r.len+=tmp[i].len;
		}
		return r;
	}
#define __CPOLL_DO_ATOI(func,str) char tmp[str.len + 1];\
														memcpy(tmp, str.d, str.len);\
														tmp[str.len] = 0;\
														return func(tmp);
	static int atoi(String s) {
		__CPOLL_DO_ATOI(::atoi, s);
	}
	static long atol(String s) {
		__CPOLL_DO_ATOI(::atol, s);
	}
	static long long atoll(String s) {
		__CPOLL_DO_ATOI(::atoll, s);
	}
	static double atof(String s) {
		__CPOLL_DO_ATOI(::atof, s);
	}

	/*
	 static long long atoq(String s) {
	 __CPOLL_DO_ATOI(::atoq, s);
	 }*/
	CP::String MemoryBuffer::String() {
		return {(char*)data(),length()};
	}
}

#endif /* BASICTYPES_H_ */
