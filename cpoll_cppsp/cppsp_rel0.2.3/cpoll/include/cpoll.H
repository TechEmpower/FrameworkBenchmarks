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
 * uses code from http://www.cse.yorku.ca/~oz/hash.html
 * for sdbm hash algorithm
 * */

/*
 * TODO:
 * 
 * * move readAll and writeAll implementation to Stream
 * * automatic AIO for file async IO
 * * socket connect()
 * * verify that all system call return values are checked
 * * verify that all code paths that may encounter exceptions
 *   	release their resources (memory etc) properly when 
 * 		exceptions are encountered
 * * HTTP support
 * 
 * 
 * */
#ifndef __INCLUDED_CPOLL_H
#define __INCLUDED_CPOLL_H
#include <string>
#include <exception>
#include <poll.h>
#include <arpa/inet.h>
#include <netinet/in.h>
#include <sys/un.h>
#include <vector>
#include <sys/signalfd.h>
#include <sys/eventfd.h>
#include <delegate.H>
#include <sys/epoll.h>
#include <limits>
#include "basictypes.H"
#include "statemachines.H"
#include <fcntl.h>
#include <math.h>

#ifndef WARNLEVEL
#define WARNLEVEL 5
#endif
#ifndef WARN
#define WARN(LEVEL,MSG) if(LEVEL<=WARNLEVEL){if(LEVEL<=1)cerr << "\x1B[41;1;33m"; else if(LEVEL<=2)cerr << "\x1B[1;1;1m"; cerr << MSG << "\x1B[0;0;0m" << endl;}
#endif
/*
 * CPoll: low level c++ wrapper for poll() and epoll(); can be implemented on
 * other OS's using mechanisms such as overlapped I/O(windows), but currently
 * there is only a linux implementation; for other OS's patches are welcome ;)
 *
 * simple usage example:
 *
 * char buf[4096];
 * char buf2[4096];
 * File f(1); //stdout
 * File f2(2); //stderr
 * f.read(buf, 4096, [](){cout << "read1 done" << endl;});
 * f2.read(buf2, 4096, [](){cout << "read2 done" << endl;});
 *
 * Poll p;
 * p.add(f);
 * p.add(f2);
 * p.loop(); //epoll is used
 *
 *
 *
 *
 * nested example:
 *
 * File f(0); //stdin
 * char buf[4096];
 * f.read([](){}, buf,4096);
 * Poll poll;
 * poll.add(f);
 * Poll poll2;
 * poll2.add(poll);
 * poll2.loop();
 */

#ifndef likely
#define likely(x)       __builtin_expect((x),1)
#define unlikely(x)     __builtin_expect((x),0)
#endif

namespace CP
{
	using namespace std;
	class CPollException: public std::exception
	{
	public:
		string message;
		int32_t number;
		CPollException();
		CPollException(int32_t number);
		CPollException(string message, int32_t number = 0);
		~CPollException() throw ();
		const char* what() const throw ();
	};
	class AbortException: public std::exception
	{ //used for aborting the event loop
	public:
		AbortException();
		~AbortException() throw ();
		const char* what() const throw ();
	};
	class CancelException: public std::exception
	{ //used for cancelling a repeat[Read|Write] operation
	  //do NOT throw this exception on one-shot operations such as read()
	public:
		CancelException();
		~CancelException() throw ();
		const char* what() const throw ();
	};
	static inline uint64_t ntohll(uint64_t value) {
		// The answer is 42
		static const int32_t num = 42;

		// Check the endianness
		if (*reinterpret_cast<const char*>(&num) == num) {
			const uint32_t high_part = htonl(static_cast<uint32_t>(value >> 32));
			const uint32_t low_part = htonl(static_cast<uint32_t>(value & 0xFFFFFFFFLL));

			return (static_cast<uint64_t>(low_part) << 32) | high_part;
		} else {
			return value;
		}
	}

	//==============================================================
	//==============================================================
	//=================NETWORK UTILITY CLASSES======================
	//=====================taken from cplib=========================
	//==============================================================
	//==============================================================
	
	struct IPAddress
	{
		in_addr a;
		inline IPAddress() {
		}
		inline IPAddress(const char* addr/*NOT hostname*/) {
			inet_pton(AF_INET, addr, &a.s_addr);
		}
		inline IPAddress(const in_addr& a) {
			this->a = a;
		}
		inline bool operator<(const IPAddress& other) const {
			return ntohl(a.s_addr) < ntohl(other.a.s_addr);
		}
		inline bool operator>(const IPAddress& other) const {
			return ntohl(a.s_addr) > ntohl(other.a.s_addr);
		}
		inline bool operator<=(const IPAddress& other) const {
			return ntohl(a.s_addr) <= ntohl(other.a.s_addr);
		}
		inline bool operator>=(const IPAddress& other) const {
			return ntohl(a.s_addr) >= ntohl(other.a.s_addr);
		}
		inline bool operator==(const IPAddress& other) const {
			return a.s_addr == other.a.s_addr;
		}
		inline IPAddress operator+(const IPAddress& other) const {
			return IPAddress( { htonl(ntohl(a.s_addr) + ntohl(other.a.s_addr)) });
		}
		inline IPAddress operator-(const IPAddress& other) const {
			return IPAddress( { htonl(ntohl(a.s_addr) - ntohl(other.a.s_addr)) });
		}
		inline IPAddress operator+(int i) const {
			//WARN(1,a.s_addr << " " <<ntohl(a.s_addr));
			//cout << "a" << endl;
			return IPAddress( { htonl(ntohl(a.s_addr) + i) });
		}
		inline IPAddress operator-(int i) const {
			return IPAddress( { htonl(ntohl(a.s_addr) - i) });
		}
		string toStr() const {
			char tmp[INET_ADDRSTRLEN];
			if (inet_ntop(AF_INET, &a, tmp, INET_ADDRSTRLEN) == NULL) throw CPollException();
			return string(tmp);
		}
	};
	struct IPv6Address
	{
		in6_addr a;
		inline IPv6Address() {
		}
		inline IPv6Address(const char* addr) {
			inet_pton(AF_INET6, addr, &a.__in6_u);
		}
		inline IPv6Address(const in6_addr& a) {
			this->a = a;
		}
		string toStr() const {
			char tmp[INET_ADDRSTRLEN];
			if (inet_ntop(AF_INET6, &a, tmp, INET6_ADDRSTRLEN) == NULL) throw CPollException();
			return string(tmp);
		}
	};
	class EndPoint: virtual public RGC::Object
	{
	public:
		int32_t addressFamily;
		virtual void getSockAddr(sockaddr* addr) const=0;
		virtual void setSockAddr(const sockaddr* addr)=0;
		virtual int32_t getSockAddrSize() const=0;
		static EndPoint* fromSockAddr(const sockaddr* addr);
		static EndPoint* create(int32_t addressFamily);
		static int getSize(int32_t addressFamily);
		static EndPoint* construct(void* mem, int32_t addressFamily);
		virtual void clone(EndPoint& to) const=0;
		virtual ~EndPoint() {
		}
		static vector<RGC::Ref<EndPoint> > lookupHost(const char* hostname, const char* port,
				int32_t family = AF_UNSPEC, int32_t socktype = 0, int32_t proto = 0, int32_t flags = 0);
		//static EndPoint Resolve(
		virtual string toStr() const=0;
	};
	class IPEndPoint: public EndPoint
	{
	public:
		IPAddress address;
		in_port_t port;
		IPEndPoint();
		IPEndPoint(IPAddress address, in_port_t port);
		void set_addr(const sockaddr_in& addr);
		virtual void setSockAddr(const sockaddr* addr);
		IPEndPoint(const sockaddr_in& addr);
		virtual void getSockAddr(sockaddr* addr) const;
		virtual int32_t getSockAddrSize() const;
		virtual void clone(EndPoint& to) const;
		virtual string toStr() const;
	};
	class IPv6EndPoint: public EndPoint
	{
	public:
		IPv6Address address;
		in_port_t port;
		uint32_t flowInfo;
		uint32_t scopeID;
		IPv6EndPoint();
		IPv6EndPoint(IPv6Address address, in_port_t port);
		void set_addr(const sockaddr_in6& addr);
		IPv6EndPoint(const sockaddr_in6& addr);
		virtual void setSockAddr(const sockaddr* addr);
		virtual void getSockAddr(sockaddr* addr) const;
		virtual int32_t getSockAddrSize() const;
		virtual void clone(EndPoint& to) const;
		virtual string toStr() const;
	};
	class UNIXEndPoint: public EndPoint
	{
	public:
		string name;
		UNIXEndPoint();
		UNIXEndPoint(string name);
		void set_addr(const sockaddr_un& addr);
		UNIXEndPoint(const sockaddr_un& addr);
		virtual void setSockAddr(const sockaddr* addr);
		virtual void getSockAddr(sockaddr* addr) const;
		virtual int32_t getSockAddrSize() const;
		virtual void clone(EndPoint& to) const;
		virtual string toStr() const;
	};
	//===========================================================
	//======================ABSTRACT CLASSES=====================
	//===========================================================
	class BufferedOutput: virtual public RGC::Object
	{
	public:
		uint8_t* buffer;
		int bufferPos;
		int bufferSize;
		BufferedOutput() {
		}
		BufferedOutput(uint8_t* buffer, int bufferPos, int bufferSize) :
				buffer(buffer), bufferPos(bufferPos), bufferSize(bufferSize) {
		}
		//flushBuffer() should flush current buffer to stream, reallocate the buffer,
		//and reset buffer, bufferPos and/or bufferSize accordingly (see below); the function
		//must reserve at least minBufferAllocation bytes of space in the write buffer
		virtual void flushBuffer(int minBufferAllocation)=0;
		void flush() {
			flushBuffer(0);
		}
	};

	class Stream: virtual public RGC::Object
	{
	public:
		Stream() = default;
		Stream(const Stream& other) = delete;
		Stream& operator=(const Stream& other) = delete;
		union
		{
			struct
			{
				DelegateBase<void(int)> cb;
				BufferedOutput* out;
				int bufSize;
				int br;
			} _readToEnd;
			struct
			{
				DelegateBase<void(int)> cb;
				BufferedOutput* out;
				int len;
				int bufSize;
				int br;
			} _readChunked;
			struct
			{
				DelegateBase<void(int)> cb;
				iovec* iov;
				int iovcnt;
				int i, br;
			} _readvAll;
			struct
			{
				DelegateBase<void(int)> cb;
				uint8_t* buf;
				int len;
				int i;
			} _readAll;
		};
		union
		{
			struct
			{
				DelegateBase<void(int)> cb;
				iovec* iov;
				int iovcnt;
				int i, br;
			} _writevAll;
			struct
			{
				DelegateBase<void(int)> cb;
				const uint8_t* buf;
				int len;
				int i;
			} _writeAll;
		};
		void _readvCB(int r);
		void _readAllCB(int r);
		void _writevCB(int r);
		void _writeAllCB(int r);

		//sync
		virtual int32_t read(void* buf, int32_t len)=0; //required
		int32_t read(String buf) {
			return read(buf.data(), buf.length());
		}
		virtual int32_t readv(iovec* iov, int iovcnt) { //optional
			if (iovcnt <= 0) return 0;
			return read(iov[0].iov_base, iov[0].iov_len);
		}
		virtual int32_t readAll(void* buf, int32_t len) { //optional
			int off = 0;
			while (off < len) {
				int tmp = read((uint8_t*) buf + off, len - off);
				if (tmp <= 0) return off;
				off += tmp;
			}
			return off;
		}
		int32_t readAll(String buf) {
			return readAll(buf.data(), buf.length());
		}
		//note: may destroy the iov array
		virtual int32_t readvAll(iovec* iov, int iovcnt) { //optional
			int i = 0;
			int br = 0;
			while (i < iovcnt) {
				int r = readv(iov + i, iovcnt - i);
				if (r <= 0) break;
				br += r;
				while (r > 0 && i < iovcnt) {
					if ((int) iov[i].iov_len > r) {
						iov[i].iov_base = ((uint8_t*) iov[i].iov_base) + r;
						iov[i].iov_len -= r;
						break;
					} else {
						r -= iov[i].iov_len;
						i++;
					}
				}
			}
			return br;
		}
		virtual int readToEnd(BufferedOutput& out, int32_t bufSize = 4096);
		virtual int readChunked(BufferedOutput& out, int32_t len, int32_t bufSize = 4096);
		virtual int32_t write(const void* buf, int32_t len)=0; //required
		int32_t write(String buf) {
			return write(buf.data(), buf.length());
		}
		virtual int32_t writev(iovec* iov, int iovcnt) { //optional
			if (iovcnt <= 0) return 0;
			return write(iov[0].iov_base, iov[0].iov_len);
		}
		virtual int32_t writeAll(const void* buf, int32_t len) { //optional
			int off = 0;
			while (off < len) {
				int tmp = write((uint8_t*) buf + off, len - off);
				if (tmp <= 0) return off;
				off += tmp;
			}
			return off;
		}
		int32_t writeAll(String buf) {
			return writeAll(buf.data(), buf.length());
		}
		virtual int32_t writevAll(iovec* iov, int iovcnt) { //optional
			int i = 0;
			int br = 0;
			while (i < iovcnt) {
				int r = writev(iov + i, iovcnt - i);
				if (r <= 0) break;
				br += r;
				while (r > 0 && i < iovcnt) {
					if ((int) iov[i].iov_len > r) {
						iov[i].iov_base = ((uint8_t*) iov[i].iov_base) + r;
						iov[i].iov_len -= r;
						break;
					} else {
						r -= iov[i].iov_len;
						i++;
					}
				}
			}
			return br;
		}

		//async
		//all new subclasses of Stream now should only need to implement the hybrid[Read|Write] functions
		//to provide async behavior; apps using [read|write](..., cb) will still work through the
		//compatibility wrappers

		//returns -2 to indicate that the operation is scheduled to be completed asynchronously; otherwise
		//the request has been completed synchronously

		//UPDATE: hybrid[Read|Write] has been canceled because it is extremely hard for users to design applications
		// to deal with 2 programming models at once and it is very error-prone
		/*virtual int32_t hybridRead(void* buf, int32_t len, const Callback& cb, bool repeat = false) { //optional (required for async)
		 }
		 virtual int32_t hybridWrite(const void* buf, int32_t len, const Callback& cb, bool repeat =
		 false) { //optional (required for async)
		 }*/
		virtual void read(void* buf, int32_t len, const Callback& cb, bool repeat = false)=0;
		void read(String buf, const Callback& cb, bool repeat = false) {
			return read(buf.data(), buf.length(), cb, repeat);
		}
		virtual void readv(iovec* iov, int iovcnt, const Callback& cb, bool repeat = false) {
			if (iovcnt <= 0) {
				cb(0);
				return;
			}
			return read(iov[0].iov_base, iov[0].iov_len, cb, repeat);
		}
		inline void repeatRead(void* buf, int32_t len, const Callback& cb) {
			read(buf, len, cb, true);
		}
		inline void repeatRead(String buf, const Callback& cb) {
			read(buf, cb, true);
		}
		inline void repeatReadv(iovec* iov, int iovcnt, const Callback& cb) {
			readv(iov, iovcnt, cb, true);
		}
		virtual void readAll(void* buf, int32_t len, const Callback& cb);
		void readAll(String buf, const Callback& cb) {
			return readAll(buf.data(), buf.length(), cb);
		}
		virtual void readvAll(iovec* iov, int iovcnt, const Callback& cb);
		virtual void readToEnd(BufferedOutput& out, const Callback& cb, int bufSize = 4096);
		virtual void readChunked(BufferedOutput& out, int32_t len, const Callback& cb, int bufSize =
				4096);

		virtual void write(const void* buf, int32_t len, const Callback& cb, bool repeat = false)=0;
		void write(String buf, const Callback& cb, bool repeat = false) {
			return write(buf.data(), buf.length(), cb, repeat);
		}
		virtual void writev(iovec* iov, int iovcnt, const Callback& cb, bool repeat = false) {
			if (iovcnt <= 0) {
				cb(0);
				return;
			}
			return write(iov[0].iov_base, iov[0].iov_len, cb, repeat);
		}
		inline void repeatWrite(const void* buf, int32_t len, const Callback& cb) {
			write(buf, len, cb, true);
		}
		inline void repeatWrite(String buf, const Callback& cb) {
			write(buf, cb, true);
		}
		inline void repeatWritev(iovec* iov, int iovcnt, const Callback& cb) {
			writev(iov, iovcnt, cb, true);
		}
		virtual void writeAll(const void* buf, int32_t len, const Callback& cb);
		void writeAll(String buf, const Callback& cb) {
			return writeAll(buf.data(), buf.length(), cb);
		}
		virtual void writevAll(iovec* iov, int iovcnt, const Callback& cb);
		//only for async read/write operations
		virtual void cancelRead()=0;
		virtual void cancelWrite()=0;

		//=========misc===========
		//sync
		virtual void close()=0; //may block
		virtual void flush()=0;

		//async
		virtual void close(const Callback& cb)=0;
		virtual void flush(const Callback& cb)=0;

		//only for streams with a read buffer; other streams should just return 0
		//calling this function should "consume" that buffer, but should not perform
		//further reading operations; this function should NOT block.
		//returns length of buffer; address of buffer is put in buf
		//if maxlen==-1, consume entire contiguous buffer
		virtual int32_t readBuffer(void*& buf, int32_t maxlen) {
			return 0;
		}
		inline String readBuffer(int32_t maxlen) {
			void* tmp = NULL;
			int32_t l = readBuffer(tmp, maxlen);
			return {(char*)tmp,l};
		}
		//freeBuffer() must be called following every readBuffer() call;
		//after calling readBuffer() and before calling freeBuffer(),
		//all other operations are undefined
		virtual void freeBuffer(void* buf, int32_t len) {
		}

		//returns NULL if it doesn't provide a buffer; in that case you need to create
		//a StreamBuffer yourself
		virtual BufferedOutput* getBufferedOutput();
	};

	class StreamBuffer: public BufferedOutput
	{
	public:
		StreamBuffer(const StreamBuffer& other) = delete;
		StreamBuffer& operator=(const StreamBuffer& other) = delete;
		RGC::Ref<Stream> output;
		StreamBuffer();
		StreamBuffer(Stream& s, int bufsize = 4096);
		virtual void flushBuffer(int minBufferAllocation);
		~StreamBuffer() {
			if (this->buffer != NULL) free(this->buffer);
		}
	}
	;
	class FixedMemoryStream;
	class MemoryStream;
	class StringStream;

	//StreamWriter will directly access the BufferedOutput's buffer
	//(for performance reasons) and increment bufferPos accordingly.
	//if bufferPos reaches bufferSize, it will call the BufferedOutput's flushBuffer() method
	class StreamWriter: public RGC::Object
	{
	public:
		StreamWriter(const StreamWriter& other) = delete;
		StreamWriter& operator=(const StreamWriter& other) = delete;
		RGC::Ref<Object> outp;
		BufferedOutput* buffer;
		char _sb[sizeof(StreamBuffer)];
		StreamBuffer& sb;
		void write(const void* s, int len) {
			if (buffer->bufferSize - buffer->bufferPos < len) buffer->flushBuffer(len);
			memcpy(buffer->buffer + buffer->bufferPos, s, len);
			buffer->bufferPos += len;
		}
		void write(const MemoryBuffer& buf) {
			write(buf.data(), buf.length());
		}
		void write(string s) {
			write(s.data(), s.length());
		}
		void write(String s) {
			write(s.data(), s.length());
		}
		void write(const char* s) {
			write((const uint8_t*) s, strlen(s));
		}
		void write(char c) {
			if (unlikely(buffer->bufferSize - buffer->bufferPos < 1)) buffer->flushBuffer(1);
			buffer->buffer[buffer->bufferPos] = c;
			buffer->bufferPos++;
		}
		void write(int8_t i) {
			//snprintf() writes a null byte and possibly a negative sign, so gotta reserve 5 bytes of buffer space
			if (unlikely(buffer->bufferSize - buffer->bufferPos < 5)) buffer->flushBuffer(5);
			buffer->bufferPos += snprintf((char*) buffer->buffer + buffer->bufferPos, 5, "%hhi", i);
		}
		void write(int16_t i) {
			if (unlikely(buffer->bufferSize - buffer->bufferPos < 7)) buffer->flushBuffer(7);
			buffer->bufferPos += snprintf((char*) buffer->buffer + buffer->bufferPos, 7, "%hi", i);
		}
		void write(int32_t i) {
			if (unlikely(buffer->bufferSize - buffer->bufferPos < 12)) buffer->flushBuffer(12);
			buffer->bufferPos += snprintf((char*) buffer->buffer + buffer->bufferPos, 12, "%i", i);
		}
		void write(int64_t i) {
			if (unlikely(buffer->bufferSize - buffer->bufferPos < 22)) buffer->flushBuffer(22);
			buffer->bufferPos += snprintf((char*) buffer->buffer + buffer->bufferPos, 22, "%lli", i);
		}
		void write(uint8_t i) {
			if (unlikely(buffer->bufferSize - buffer->bufferPos < 4)) buffer->flushBuffer(4);
			buffer->bufferPos += snprintf((char*) buffer->buffer + buffer->bufferPos, 4, "%hhu", i);
		}
		void write(uint16_t i) {
			if (unlikely(buffer->bufferSize - buffer->bufferPos < 6)) buffer->flushBuffer(6);
			buffer->bufferPos += snprintf((char*) buffer->buffer + buffer->bufferPos, 6, "%hu", i);
		}
		void write(uint32_t i) {
			if (unlikely(buffer->bufferSize - buffer->bufferPos < 11)) buffer->flushBuffer(11);
			buffer->bufferPos += snprintf((char*) buffer->buffer + buffer->bufferPos, 11, "%u", i);
		}
		void write(uint64_t i) {
			if (unlikely(buffer->bufferSize - buffer->bufferPos < 21)) buffer->flushBuffer(21);
			buffer->bufferPos += snprintf((char*) buffer->buffer + buffer->bufferPos, 21, "%llu", i);
		}
		char* beginWrite(int len) {
			if (unlikely(buffer->bufferSize - buffer->bufferPos < len)) buffer->flushBuffer(len);
			return (char*) buffer->buffer + buffer->bufferPos;
		}
		void endWrite(int len) {
			buffer->bufferPos += len;
		}
		template<class ... P> void writeF(const char* fmt, P&&... p) {
			int minSpace = 0;
			if (minSpace + 1 > buffer->bufferSize - buffer->bufferPos) {
				redo: buffer->flushBuffer(minSpace + 1);
			}
			minSpace = snprintf((char*) buffer->buffer + buffer->bufferPos,
					buffer->bufferSize - buffer->bufferPos, fmt, std::forward<P>(p)...);
			if (minSpace + 1 > buffer->bufferSize - buffer->bufferPos) goto redo;
			buffer->bufferPos += minSpace;
		}
		void flush() {
			buffer->flushBuffer(0);
		}
		StreamWriter(BufferedOutput& s);
		StreamWriter(Stream& s);
		StreamWriter(MemoryStream& s);
		StreamWriter(StringStream& s);
		~StreamWriter();
	};

	class StreamReader: public Stream
	{ //writes are simply passed through
	public:
		typedef Delegate<void(const string&)> Callback;
		typedef Delegate<void(int)> StreamCallback;
		RGC::Ref<Stream> input;
		//void* sr;
		newStreamReader _sr;
		bool* deletionFlag;
		Stream* out_s;
		union
		{
			DelegateBase<void(const string&)> cb;
			DelegateBase<void(int)> cb_s;
		};
		//BufferedOutput* tmp_out;
		string tmp;
		string tmp_delim;

		int tmp_i;
		int bufSize;

		bool eof;
		/*void* curBuffer;
		 int curBufferLen;
		 int bufferSize;
		 bool bufferIsBorrowed;*/

		StreamReader(Stream& input, int bufsize = 4096);
		~StreamReader();

		string readTo(char delim);
		string readTo(const char* delim, int delimLen);
		string readTo(string delim);
		string readLine();
		int readTo(char delim, Stream& s);
		int readTo(const char* delim, int delimLen, Stream& s);
		int readTo(string delim, Stream& s);
		int readLine(Stream& s);

		void readTo(char delim, const Callback& cb);
		//*delim MUST NOT BE DELETED FOR THE ENTIRE DURATION OF THE READTO REQUEST!!!!!
		//it does not make a copy of delim!!!!!
		void readTo(const char* delim, int delimLen, const Callback& cb);
		void readTo(string delim, const Callback& cb);
		void readLine(const Callback& cb);

		void readTo(char delim, Stream& s, const StreamCallback& cb);
		void readTo(const char* delim, int delimLen, Stream& s, const StreamCallback& cb);
		void readTo(string delim, Stream& s, const StreamCallback& cb);
		void readLine(Stream& s, const StreamCallback& cb);

		//sync
		virtual int32_t read(void* buf, int32_t len);
		virtual int32_t write(const void* buf, int32_t len) {
			return input->write(buf, len);
		}
		virtual int32_t writeAll(const void* buf, int32_t len) {
			return input->writeAll(buf, len);
		}

		//async
		virtual void read(void* buf, int32_t len, const CP::Callback& cb, bool repeat = false);
		virtual void readAll(void* buf, int32_t len, const CP::Callback& cb);
		virtual void write(const void* buf, int32_t len, const CP::Callback& cb,
				bool repeat = false) {
			return input->write(buf, len, cb, repeat);
		}
		virtual void writeAll(const void* buf, int32_t len, const CP::Callback& cb) {
			return input->writeAll(buf, len, cb);
		}
		//does NOT cancel readTo() or readLine() operations
		virtual void cancelRead();
		virtual void cancelWrite();

		//=========misc===========
		//sync
		virtual void close();
		virtual void flush();

		//async
		virtual void close(const CP::Callback& cb);
		virtual void flush(const CP::Callback& cb);

		virtual int32_t readBuffer(void*& buf, int32_t maxlen);
		virtual void freeBuffer(void* buf, int32_t len);

		//internal
		bool _loop(bool);
		void _beginRead();
		void _doSyncRead();
		void _readCB(int i);
	}
	;

	//===========================================================
	//======================UTILITY CLASSES======================
	//===========================================================

	//===========================================================
	//======================DATA STRUCTURES======================
	//===========================================================
	
	enum class Events
		: event_t
		{
			none = 0, in = 1, out = 2, other = 4, all = 7
	}
	;
	static inline Events operator&(Events e1, Events e2) {
		return (Events) (((event_t) e1) & ((event_t) e2));
	}
	static inline Events operator|(Events e1, Events e2) {
		return (Events) (((event_t) e1) | ((event_t) e2));
	}
	static inline Events operator^(Events e1, Events e2) {
		return (Events) (((event_t) e1) ^ ((event_t) e2));
	}
	static inline const Events& operator&=(Events& e1, Events e2) {
		return (Events&) (((event_t&) e1) &= ((event_t) e2));
	}
	static inline const Events& operator|=(Events& e1, Events e2) {
		return (Events&) (((event_t&) e1) |= ((event_t) e2));
	}
	static inline const Events& operator^=(Events& e1, Events e2) {
		return (Events&) (((event_t&) e1) ^= ((event_t) e2));
	}

	enum class Operations
		: uint8_t
		{
			none = 0, read = 1, write, send, recv, sendTo, recvFrom, accept, readAll, writeAll,
		sendAll, recvAll, shutdown, connect, close, readv, writev, lastItem
	};

	struct EventData
	{
	public:
		bool hungUp, error;
	};
	struct EventHandlerData
	{
	public:
		Callback cb;
		union miscUnion
		{
			struct
			{
				void* buf;
				union
				{
					EndPoint* ep; //recvFrom
					const EndPoint* const_ep; //sendTo
				};
				int32_t len;
				int32_t flags;
				int32_t len_done;
			} bufferIO;
			struct
			{
				iovec* iov;
				int iovcnt;
			} bufferIOv;
			struct
			{
				int32_t how;
			} shutdown;
			struct
			{
				eventfd_t evt;
			} eventfd;
		} misc;
		Delegate<bool(Events event, EventHandlerData& ed, const EventData& evtd, bool confident)> opcb;
		Operations op;
		enum class States
			: uint8_t {
				invalid = 0, once = 1, repeat
		} state;
		EventHandlerData() :
				state(States::invalid) {
		}
	};
	
	static const int32_t numEvents = 2;
	//============================================================
	//============================================================
	//=======================MAIN CLASSES=========================
	//============================================================
	//============================================================
	
	class Handle: virtual public RGC::Object
	{
	public:
		Handle(const Handle& other) = delete;
		Handle& operator=(const Handle& other) = delete;
		//void* __private;
		HANDLE handle;
		bool _supportsEPoll;
		Handle();
		Handle(HANDLE handle);
		Delegate<void(Handle& h, Events old_events)> onEventsChange;
		Delegate<void(Handle& h)> onClose;

		virtual void init(HANDLE handle);
		virtual void deinit();
		///calls the callback associated with the event
		///only accepts one event
		virtual bool dispatch(Events event, const EventData& evtd, bool confident)=0;
		virtual Events getEvents()=0;
		virtual Events dispatchMultiple(Events events, Events confident, const EventData& evtd);
		///get some events from the queue.
		virtual Events wait(EventData& evtd);
		virtual Events waitAndDispatch();
		virtual void loop();
		void setBlocking(bool b = true) {
			int f = fcntl(handle, F_GETFL);
			if (b && (f & O_NONBLOCK)) {
				fcntl(handle, F_SETFL, f & ~O_NONBLOCK);
			} else if (!b && (f & O_NONBLOCK) == 0) {
				fcntl(handle, F_SETFL, f | O_NONBLOCK);
			}
		}
		//void close();
		~Handle();
	};
	class File: public Handle, public Stream
	{
	public:
		EventHandlerData eventData[numEvents];
		bool* deletionFlag;
		Events preDispatchEvents;
		bool dispatching;

		File();
		File(HANDLE handle);
		virtual void init(HANDLE handle);
		Events _getEvents();
		///only accepts one event
		EventHandlerData* beginAddEvent(Events event);
		void endAddEvent(Events event, bool repeat = false);
		void cancel(Events event);
		int32_t read(void* buf, int32_t len) override;
		int32_t readv(iovec* iov, int iovcnt) override;
		int32_t readAll(void* buf, int32_t len) {
			return Stream::readAll(buf, len);
		}
		int32_t write(const void* buf, int32_t len) override;
		int32_t writev(iovec* iov, int iovcnt) override;
		int32_t writeAll(const void* buf, int32_t len) {
			return Stream::writeAll(buf, len);
		}
		int32_t send(const void* buf, int32_t len, int32_t flags = 0);
		int32_t sendAll(const void* buf, int32_t len, int32_t flags = 0);
		int32_t recv(void* buf, int32_t len, int32_t flags = 0);
		int32_t recvAll(void* buf, int32_t len, int32_t flags = 0);
		Events checkEvents(Events events);
		virtual bool doOperation(Events event, EventHandlerData& ed, const EventData& evtd,
				EventHandlerData::States oldstate, bool confident);
		bool dispatch(Events event, const EventData& evtd, bool confident, bool& deletionFlag);
		bool dispatch(Events event, const EventData& evtd, bool confident) override {
			bool d = false;
			deletionFlag = &d;
			bool r = dispatch(event, evtd, d);
			if (!d) deletionFlag = NULL;
			return r;
		}
		Events dispatchMultiple(Events events, Events confident, const EventData& evtd) override;
		inline void fillIOEventHandlerData(EventHandlerData* ed, void* buf, int32_t len,
				const Callback& cb, Events e, Operations op);
		inline void fillIOEventHandlerData(EventHandlerData* ed, iovec* iov, int iovcnt,
				const Callback& cb, Events e, Operations op);
		void read(void* buf, int32_t len, const Callback& cb, bool repeat = false) override;
		void readv(iovec* iov, int iovcnt, const Callback& cb, bool repeat = false) override;
		void readAll(void* buf, int32_t len, const Callback& cb) override;

		void write(const void* buf, int32_t len, const Callback& cb, bool repeat = false) override;
		void writev(iovec* iov, int iovcnt, const Callback& cb, bool repeat = false) override;
		void writeAll(const void* buf, int32_t len, const Callback& cb) override;

		void recv(void* buf, int32_t len, int32_t flags, const Callback& cb, bool repeat = false);
		inline void repeatRecv(void* buf, int32_t len, int32_t flags, const Callback& cb) {
			recv(buf, len, flags, cb, true);
		}
		void recvAll(void* buf, int32_t len, int32_t flags, const Callback& cb);

		void send(const void* buf, int32_t len, int32_t flags, const Callback& cb,
				bool repeat = false);
		inline void repeatSend(const void* buf, int32_t len, int32_t flags, const Callback& cb) {
			send(buf, len, flags, cb, true);
		}
		void sendAll(const void* buf, int32_t len, int32_t flags, const Callback& cb);
		virtual Events getEvents() override final {
			return _getEvents();
		}
		~File();

		//=========misc===========
		//sync
		virtual void close(); //may block
		virtual void flush();

		//async
		virtual void close(const Callback& cb);
		virtual void flush(const Callback& cb);

		//misc
		void cancelRead();
		void cancelWrite();
		void cancelSend() {
			cancelWrite();
		}
		void cancelRecv() {
			cancelRead();
		}
		void waitForEvent(Events event, const Callback& cb, bool repeat = false);
	};
	class Socket: public File
	{
	public:
		union
		{
			DelegateBase<void(Socket*)> _acceptCB;
			DelegateBase<void(HANDLE)> _acceptHandleCB;
		};
		//Delegate<void(Socket*)> _acceptCB;
		int32_t addressFamily, type, protocol;
		//RGC::Ref<EndPoint> peer;
		Socket();
		Socket(HANDLE handle, int32_t d, int32_t t, int32_t p);
		Socket(int32_t d, int32_t t = SOCK_STREAM, int32_t p = 0);
		virtual void init(HANDLE handle, int32_t d, int32_t t, int32_t p);
		virtual void init(int32_t d, int32_t t, int32_t p);

		//the caller must release() or free() the returned object
		EndPoint* getLocalEndPoint();
		//the caller must release() or free() the returned object
		EndPoint* getRemoteEndPoint();
		virtual bool doOperation(Events event, EventHandlerData& ed, const EventData& evtd,
				EventHandlerData::States oldstate, bool confident) override;

		void connect(const sockaddr *addr, int32_t addr_size);
		void connect(const EndPoint &ep);
		void connect(const char* hostname, const char* port, int32_t family = AF_UNSPEC,
				int32_t socktype = 0, int32_t proto = 0, int32_t flags = 0);
		void bind(const sockaddr *addr, int32_t addr_size);
		void bind(const EndPoint &ep);
		//initsock is called right after creating the socket; you can use it to set socket options
		//such as SO_REUSEPORT that need to be set before binding
		void bind(const char* hostname, const char* port, int32_t family = AF_UNSPEC,
				int32_t socktype = 0, int32_t proto = 0, int32_t flags = 0,
				Callback initsock = nullptr);

		int32_t shutdown(int32_t how);
		void shutdown(int32_t how, const Callback& cb);
		void listen(int32_t backlog = 8);
		//the caller must release() or free() the returned object
		Socket* accept();
		HANDLE acceptHandle();

		void connect(const sockaddr *addr, int32_t addr_size, const Callback& cb);
		void connect(const EndPoint &ep, const Callback& cb);
		void connect(const char* hostname, const char* port, const Callback& cb, int32_t family =
				AF_UNSPEC, int32_t socktype = 0, int32_t proto = 0, int32_t flags = 0);

		//callback function must release() or free() the received object
		void accept(const Delegate<void(Socket*)>& cb, bool repeat = false);
		void acceptHandle(const Delegate<void(HANDLE)>& cb, bool repeat = false);
		inline void repeatAccept(const Delegate<void(Socket*)>& cb) {
			accept(cb, true);
		}
		inline void repeatAcceptHandle(const Delegate<void(HANDLE)>& cb) {
			acceptHandle(cb, true);
		}

		int32_t recvFrom(void* buf, int32_t len, int32_t flags, EndPoint& ep);
		int32_t sendTo(const void* buf, int32_t len, int32_t flags, const EndPoint& ep);
		//ep has to remain valid for the entire duration of the request
		void recvFrom(void* buf, int32_t len, int32_t flags, EndPoint& ep, const Callback& cb,
				bool repeat = false);
		inline void repeatRecvFrom(void* buf, int32_t len, int32_t flags, EndPoint& ep,
				const Callback& cb) {
			recvFrom(buf, len, flags, ep, cb, true);
		}
		void sendTo(const void* buf, int32_t len, int32_t flags, const EndPoint& ep,
				const Callback& cb, bool repeat = false);
		inline void repeatSendTo(const void* buf, int32_t len, int32_t flags, const EndPoint& ep,
				const Callback& cb) {
			sendTo(buf, len, flags, ep, cb, true);
		}
	};
	class SignalFD: public Handle
	{
	public:
		static int32_t MAX_EVENTS;
		typedef struct signalfd_siginfo Signal;
		Delegate<void(Signal& sig)> callback;
		sigset_t mask;
		SignalFD(HANDLE handle, const sigset_t& mask);
		SignalFD(const sigset_t& mask, int32_t flags);
		virtual bool dispatch(Events event, const EventData& evtd, bool confident) override;
		virtual Events getEvents();
	};
	class Timer: public Handle
	{
	public:
		static int32_t MAX_EVENTS;
		Callback cb;
		struct timespec interval;
		bool* deletionFlag;
		bool dispatching;
		//if interval is 0, timer is disabled; timer is always recurring (unless disabled)
		void setInterval(struct timespec interval);
		void setInterval(uint64_t interval_ms);
		void init(HANDLE handle);
		void init(HANDLE handle, struct timespec interval);
		void init(HANDLE handle, uint64_t interval_ms);
		void init(struct timespec interval);
		void init(uint64_t interval_ms = 0);
		void close();
		Timer(HANDLE handle);
		Timer(HANDLE handle, uint64_t interval_ms);
		Timer(HANDLE handle, struct timespec interval);
		Timer(uint64_t interval_ms = 0);
		Timer(struct timespec interval);
		~Timer();
		struct timespec getInterval();
		bool running();
		void setCallback(const Callback& cb);
		virtual bool dispatch(Events event, const EventData& evtd, bool confident) override;
		virtual Events getEvents();
	};
	class EventFD: public File
	{
	public:
		Delegate<void(eventfd_t)> cb;
		EventFD(HANDLE handle);
		EventFD(uint32_t initval = 0, int32_t flags = 0);
		virtual bool doOperation(Events event, EventHandlerData& ed, const EventData& evtd,
				EventHandlerData::States oldstate, bool confident) override;
		eventfd_t getEvent();
		void getEvent(const Delegate<void(eventfd_t)>& cb, bool repeat = false);
		void repeatGetEvent(const Delegate<void(eventfd_t)>& cb) {
			getEvent(cb, true);
		}
		int32_t sendEvent(eventfd_t evt = 1);
		void sendEvent(eventfd_t evt, const Delegate<void(int32_t)>& cb);
	};
	//XXX: AIO support in the linux kernel is incomplete, and
	//	has many serious limitations such as:
	//	- files have to be opened as O_DIRECT
	//	- O_DIRECT implies that all I/O requests have to
	//		be block-aligned
	//Because of the said reasons, AIO will not be implemented
	//for now
	/*class AIO: public SignalFD
	 {

	 };*/

	//epoll wrapper
	class EPoll: public Handle
	{
	public:
		static int32_t MAX_EVENTS;
		epoll_event* curEvents;
		int32_t curIndex, curLength;
		int32_t active;
		HANDLE cur_handle;
		bool cur_deleted;
		//bool debug;
		//Events cur_last_events;
		//map<HANDLE, Ref<Handle> > handles;
		//set<Handle*> tmp_deleted;
		//bool has_deleted;
		EPoll(HANDLE handle);
		EPoll();
		int32_t _doEPoll(int32_t timeout);
		virtual bool dispatch(Events event, const EventData& evtd, bool confident) override;
		virtual Events dispatchMultiple(Events event, Events confident, const EventData& evtd)
				override;
		virtual Events getEvents() override;
		virtual Events waitAndDispatch() override;

		inline void applyHandle(Handle& h, Events old_e);
		void add(Handle& h);
		void del(Handle& h);
	};
	class NewEPoll: public Handle
	{
	public:
		static int32_t MAX_EVENTS;
		struct drainInfo
		{
			Handle* h;
			Events new_e;
		};
		vector<drainInfo> _pending;
		vector<drainInfo>* _draining;
		Handle* _dispatchingHandle;
		epoll_event* _curEvents;
		int32_t _curIndex, _curLength;
		bool _dispatchingDeleted;
		NewEPoll(HANDLE h);
		NewEPoll();
		virtual bool dispatch(Events event, const EventData& evtd, bool confident) override;
		virtual Events dispatchMultiple(Events event, Events confident, const EventData& evtd)
				override;
		virtual Events getEvents() override;
		virtual Events waitAndDispatch() override;
		void add(Handle& h);
		void del(Handle& h);
		bool _doIteration(int timeout);
		void _doDispatch(const epoll_event& event);
		void _drainHandle(Handle& h, Events new_e);
		void _queueHandle(Handle& h, Events new_e);
		void _applyHandle(Handle& h, Events old_e);
	};
	typedef NewEPoll Poll;
	class StandardStream: public Stream
	{
	public:
		StandardStream(const StandardStream& other) = delete;
		StandardStream& operator=(const StandardStream& other) = delete;
		CP::File in, out;
		StandardStream();
		template<class P> void addToPoll(P& p) {
			p.add(in);
			p.add(out);
		}
		template<class P> void delFromPoll(P& p) {
			p.del(in);
			p.del(out);
		}
		void setBlocking(bool b = true) {
			in.setBlocking(b);
			out.setBlocking(b);
		}
		//sync
		virtual int32_t read(void* buf, int32_t len);
		virtual int32_t readAll(void* buf, int32_t len);
		virtual int32_t write(const void* buf, int32_t len);
		virtual int32_t writeAll(const void* buf, int32_t len);

		//async
		virtual void read(void* buf, int32_t len, const Callback& cb, bool repeat = false);
		virtual void readAll(void* buf, int32_t len, const Callback& cb);
		virtual void write(const void* buf, int32_t len, const Callback& cb, bool repeat = false);
		virtual void writeAll(const void* buf, int32_t len, const Callback& cb);
		virtual void cancelRead();
		virtual void cancelWrite();

		//=========misc===========
		//sync
		virtual void close(); //may block
		virtual void flush();

		//async
		virtual void close(const Callback& cb);
		virtual void flush(const Callback& cb);
	};

	class FixedMemoryStream: public Stream, public BufferedOutput, public MemoryBuffer
	{
	public:
		FixedMemoryStream(const FixedMemoryStream& other) = delete;
		FixedMemoryStream& operator=(const FixedMemoryStream& other) = delete;
		//uint8_t* data;
		//int len, pos;
		int len;
		FixedMemoryStream();
		FixedMemoryStream(void* data, int len);
		//sync
		virtual int32_t read(void* buf, int32_t len);
		virtual int32_t readAll(void* buf, int32_t len);
		virtual int32_t write(const void* buf, int32_t len);
		virtual int32_t writeAll(const void* buf, int32_t len);

		//async
		virtual void read(void* buf, int32_t len, const Callback& cb, bool repeat = false);
		virtual void readAll(void* buf, int32_t len, const Callback& cb);
		virtual void write(const void* buf, int32_t len, const Callback& cb, bool repeat = false);
		virtual void writeAll(const void* buf, int32_t len, const Callback& cb);
		virtual void cancelRead();
		virtual void cancelWrite();

		//=========misc===========
		//sync
		virtual void close();
		virtual void flush();

		//async
		virtual void close(const Callback& cb);
		virtual void flush(const Callback& cb);

		virtual int32_t readBuffer(void*& buf, int32_t maxlen);
		virtual void flushBuffer(int minBufferAllocation);

		virtual BufferedOutput* getBufferedOutput() override;

		inline virtual uint8_t* data() const final {
			return buffer;
		}
		inline virtual int length() const final {
			return len;
		}
	};
	class MemoryStream: public FixedMemoryStream
	{
	public:
		MemoryStream(int capacity = 4096);
		~MemoryStream();
		void ensureCapacity(int c);
		virtual int32_t write(const void* buf, int32_t len);
		virtual int32_t writeAll(const void* buf, int32_t len);
		virtual void close();
		virtual void flush() {
			this->flushBuffer(0);
		}
		void setLength(int l) {
			len = l;
		}
		void setPosition(int i) {
			bufferPos = i;
		}
		void clear();
		virtual void flushBuffer(int minBufferAllocation);
		//user must delete the MemoryStream instance after this call
		void keepBuffer();

		//allocates space in the buffer after the current position
		CP::String beginAppend(int minLen) {
			ensureCapacity(bufferPos + minLen);
			return {(char*)buffer+bufferPos,bufferSize-bufferPos};
		}
		void endAppend(int len) {
			if (bufferPos + len > bufferSize) throw overflow_error(
					"invalid length parameter to MemoryStream::endAppend()");
			bufferPos += len;
			if (bufferPos > this->len) this->len = bufferPos;
		}
	};
	class StringPool: public virtual RGC::Object, public RGC::Allocator
	{
	public:
		StringPool(const StringPool& other) = delete;
		StringPool& operator=(const StringPool& other) = delete;
		struct _pageHeader
		{
			_pageHeader* next;
		};
		struct state
		{
			_pageHeader* _curPage;
			_pageHeader* _curRawItem;
			int _curIndex;
		};
		_pageHeader* _firstPage;
		_pageHeader* _curPage;
		_pageHeader* _firstRawItem;
		_pageHeader* _curRawItem;
		int _pageSize;
		int _curIndex;
		StringPool(int pageSize = 4096);
		~StringPool();
		char* beginAdd(int sz) {
#ifdef STRINGPOOL_MEMORY_DIAG
			int length = sz + sizeof(int) + 1;
#else
			int length = sz + 1;
#endif
			if (likely(_curPage!=NULL && length<=(_pageSize - (int) sizeof(_pageHeader) - _curIndex))) {
				sssss:
				//insert null byte at the end
				*(((char*) (_curPage + 1)) + _curIndex + sz) = 0;
#ifdef STRINGPOOL_MEMORY_DIAG
				int* ptr = (int*) (((char*) (_curPage + 1)) + _curIndex);
				*ptr = sz;
				memset(ptr + 1, 0xff, sz);
				return (char*) (ptr + 1);
#else
				return ((char*) (_curPage + 1)) + _curIndex;
#endif
			}
			if (length > (_pageSize - (int) sizeof(_pageHeader)) / 2) {
				_addRaw(length);
				return ((char*) (_curRawItem + 1));
			}
			_addPage();
			goto sssss;
		}
		void endAdd(int length) {
#ifdef STRINGPOOL_MEMORY_DIAG
			_curIndex += length + sizeof(int) + 1;
#else
			_curIndex += length + 1;
#endif
		}
		char* add(int length) {
			char* tmp = beginAdd(length);
			endAdd(length);
			return tmp;
		}
		char* add(const char* s, int length) {
			char* tmp = beginAdd(length);
			memcpy(tmp, s, length);
			endAdd(length);
			return tmp;
		}
		char* add(String s) {
			return add(s.data(), s.length());
		}
		char* add(const char* s) {
			return add(s, strlen(s));
		}
		String addString(int length) {
			return {add(length),length};
		}
		String addString(const char* s, int length) {
			return {add(s,length),length};
		}
		String addString(String s) {
			return {add(s.data(), s.length()),s.length()};
		}
		String addString(const char* s) {
			int l = strlen(s);
			return {add(s, l),l};
		}
		void* alloc(int s) override final {
			return add(s);
		}
		void dealloc(void* obj) override final {
#ifdef STRINGPOOL_MEMORY_DIAG
			int* tmp = (int*) obj;
			tmp--;
			memset(obj, 0xFE, *tmp);
#endif
		}
		void clear();
		state saveState() {
			return {_curPage,_curRawItem,_curIndex};
		}
		//deallocate all blocks allocated after s was saved
		void restoreState(state s) {
			_pageHeader* h;
			if (s._curPage != NULL) {
				h = s._curPage->next;
				s._curPage->next = NULL;
				while (h != NULL) {
					_pageHeader* n = h->next;
					::free(h);
					h = n;
				}
			}
			if (s._curRawItem != NULL) {
				h = s._curRawItem->next;
				s._curRawItem->next = NULL;
				while (h != NULL) {
					_pageHeader* n = h->next;
					::free(h);
					h = n;
				}
			}
			_curPage = s._curPage;
			_curRawItem = s._curRawItem;
			_curIndex = s._curIndex;
		}
		void _addPage();
		void _addRaw(int len);
	};
	template<class T>
	class PoolAllocator
	{
	public:
		typedef size_t size_type;
		typedef ptrdiff_t difference_type;
		typedef T* pointer;
		typedef const T* const_pointer;
		typedef T& reference;
		typedef const T& const_reference;
		typedef T value_type;

		StringPool* sp;
		template<class U>
		struct rebind
		{
			typedef PoolAllocator<U> other;
		};
		PoolAllocator(StringPool* sp) throw () :
				sp(sp) {

		}
		template<class U>
		PoolAllocator(const PoolAllocator<U>& other) :
				sp(other.sp) {

		}
		~PoolAllocator() {
		}
		template<class U>
		PoolAllocator& operator=(const PoolAllocator<U>& other) throw () {
			sp = other.sp;
			return *this;
		}
		//    address
		inline pointer address(reference r) {
			return &r;
		}
		inline const_pointer address(const_reference r) {
			return &r;
		}
		//    memory allocation
		inline pointer allocate(size_type cnt, typename std::allocator<void>::const_pointer = 0) {
			int size = cnt * sizeof(T);
			pointer p = (pointer) sp->add(size);
			//printf("allocate(size=%i): %p\n", size, p);
			return p;
		}
		inline void deallocate(pointer p, size_type) {

		}
		//    size
		inline size_type max_size() const {
			return std::numeric_limits<size_type>::max() / sizeof(T);
		}
		//    construction/destruction
		inline void construct(pointer p, const T& t) {
			new (p) T(t);
		}
		inline void destroy(pointer p) {
			p->~T();
		}
		inline bool operator==(PoolAllocator const&) {
			return true;
		}
		inline bool operator!=(PoolAllocator const& a) {
			return !operator==(a);
		}
	};
	class StringStream: public Stream, public BufferedOutput, public MemoryBuffer
	{
	public:
		StringStream(const StringStream& other) = delete;
		StringStream& operator=(const StringStream& other) = delete;
		//uint8_t* data;
		//int len, pos;
		string _str;
		int len;
		StringStream();
		//sync
		virtual int32_t read(void* buf, int32_t len);
		virtual int32_t readAll(void* buf, int32_t len);
		virtual int32_t write(const void* buf, int32_t len);
		virtual int32_t writeAll(const void* buf, int32_t len);

		//async
		virtual void read(void* buf, int32_t len, const Callback& cb, bool repeat = false);
		virtual void readAll(void* buf, int32_t len, const Callback& cb);
		virtual void write(const void* buf, int32_t len, const Callback& cb, bool repeat = false);
		virtual void writeAll(const void* buf, int32_t len, const Callback& cb);
		virtual void cancelRead();
		virtual void cancelWrite();

		//=========misc===========
		//sync
		virtual void close();
		virtual void flush();

		//async
		virtual void close(const Callback& cb);
		virtual void flush(const Callback& cb);

		virtual int32_t readBuffer(void*& buf, int32_t maxlen);
		virtual void flushBuffer(int minBufferAllocation);

		virtual BufferedOutput* getBufferedOutput() override;
		void clear();
		inline string str() const {
			return _str.substr(0, len);
		}
		inline virtual uint8_t* data() const final {
			return buffer;
		}
		inline virtual int length() const final {
			return len;
		}
		CP::String beginAppend(int minLen) {
			_str.reserve(bufferPos + minLen);
			_str.resize(_str.capacity());
			this->buffer = (uint8_t*) _str.data();
			return {(char*)buffer+bufferPos,bufferSize-bufferPos};
		}
		void endAppend(int len) {
			if (bufferPos + len > bufferSize) throw overflow_error(
					"invalid length parameter to MemoryStream::endAppend()");
			bufferPos += len;
			if (bufferPos > this->len) this->len = bufferPos;
		}
	};
	void listDirectory(const char* path, Delegate<void(const char*)> cb);

	class MemoryPool: public RGC::Allocator
	{
	public:
		struct _item
		{
			_item* nextFree;
		};
		_item* _freeList;
		_item* _lastFree;
		int size;
		int items;
		int maxItems;
		MemoryPool(int size, int maxItems = 1024);
		MemoryPool(const MemoryPool& other) = delete;
		~MemoryPool();
		void* alloc();
		void* alloc(int s) override;
		void dealloc(void* obj) override;
		MemoryPool& operator=(const MemoryPool& other) = delete;
	};
	//all mutexes are assumed to be recursive
	class Mutex
	{
	public:
		virtual void lock()=0;
		virtual void unlock()=0;
	};
	class ScopeLock
	{
	public:
		Mutex* mutex;
		ScopeLock(Mutex& m) :
				mutex(&m) {
			m.lock();
		}
		ScopeLock(const ScopeLock& l) = delete;
		void earlyUnlock() {
			if (mutex != nullptr) mutex->unlock();
			mutex = nullptr;
		}
		~ScopeLock() {
			if (mutex != nullptr) mutex->unlock();
		}
		ScopeLock& operator=(const ScopeLock& other) = delete;
	};
	class PThreadMutex: public Mutex
	{
	public:
		pthread_mutex_t m;
		PThreadMutex();
		~PThreadMutex();
		void lock() override;
		void unlock() override;
	};

	//reference counted buffer
	class Buffer: public String
	{
	public:
		typedef int refc_t;
		refc_t* pbuf;
		inline void __inc() {
			++*pbuf;
		}
		inline void __dec_autofree() {
			if (--*pbuf <= 0) free(pbuf);
		}
		inline Buffer() :
				pbuf(NULL) {
		}
		Buffer(refc_t* orig, void* buf, int length) :
				String((char*) buf, length), pbuf(orig) {
			__inc();
		}
		inline void release() {
			if (pbuf != NULL) __dec_autofree();
			pbuf = NULL;
			d = NULL;
			len = 0;
		}
		Buffer(int length) :
				String((char*) NULL, length) {
			if (length <= 0) {
				this->pbuf = NULL;
				return;
			}
			this->pbuf = (refc_t*) malloc(length + sizeof(refc_t));
			if (this->pbuf == NULL) throw bad_alloc();
			*this->pbuf = 1;
			this->d = (char*) (this->pbuf + 1);
		}
		Buffer(const Buffer& b) :
				String(b), pbuf(b.pbuf) {
			if (b.pbuf != NULL) __inc();
		}
		Buffer& operator=(const Buffer& b) {
			String::operator=(b);
			if (pbuf != NULL) __dec_autofree();
			if ((pbuf = b.pbuf) != NULL) __inc();
			return *this;
		}
		inline ~Buffer() {
			if (pbuf != NULL) __dec_autofree();
		}
		inline Buffer subBuffer(int index, int length) const {
			if (index < 0 || index + length > this->len || length < 0) throw range_error(
					"Buffer::subBuffer() out of range");
			return Buffer(pbuf, d + index, length);
		}
		inline Buffer subBuffer(int index) const {
			return subBuffer(index, len - index);
		}
	};
	class BitArray
	{
	public:
		uint32_t* data;
		int length;
		BitArray(int l) :
				length(l) {
			data = new uint32_t[(int) ceil(length / 32)];
		}
		~BitArray() {
			delete[] data;
		}
		inline bool get(int i) { //WARNING: no runtime check
			__uint32_t tmp = data[i / 32];
			return (tmp & ((int) 1 << (i % 32))) != 0;
		}
		inline void set(int i, bool b) {
			int tmp1 = i / 32;
			__uint32_t tmp = data[tmp1];
			if (b) data[tmp1] = tmp | ((int) 1 << (i % 32));
			else data[tmp1] = tmp & (~((int) 1 << (i % 32)));
		}
	};
	template<class T> class CircularQueue: public RGC::Object
	{
	public:
		T* array;
		BitArray b;
		int size;
		int objsize;
		int __wrap;
		int s1, s2, e1, e2;
		CircularQueue(int size, int objsize = 1) :
				b(size), size(size), objsize(objsize), __wrap(size * 2), s1(0), s2(0), e1(0), e2(0) {
			array = new T[size * objsize];
		}
		~CircularQueue() {
			delete[] array;
		}
		inline int __getlength(int i1, int i2, int wrap) {
			return (i2 < i1 ? i2 + wrap : i2) - i1;
		}
		inline T& getPointer(int i) {
			i %= size;
			if (i >= size || i < 0) throw out_of_range("CircularQueue::getPointer() out of range");
			return array[i * objsize]; //__intwrap(i,size);
		}
		inline bool canAppend() {
			return __getlength(s1, e2, __wrap) < size;
		}
		int beginAppend() {
			if (__getlength(s1, e2, __wrap) >= size) return -1;
			int tmp = e2++;
			e2 %= __wrap;
			b.set(tmp % size, true);
			return tmp;
		}
		void endAppend(int i) {
			if (i == e1) {
				do {
					e1++;
					e1 %= __wrap;
				} while (__getlength(e1, e2, __wrap) > 0 && !(b.get(e1 % size)));
			} else b.set(i % size, false);
		}
		inline int length() {
			return __getlength(s2, e1, __wrap);
		}
		inline bool canDequeue() {
			return __getlength(s2, e1, __wrap) > 0;
		}
		int beginDequeue() {
			if (__getlength(s2, e1, __wrap) <= 0) return -1;
			int tmp = s2++;
			s2 %= __wrap;
			b.set(tmp % size, true);
			return tmp;
		}
		void endDequeue(int i) {
			if (i == s1) {
				do {
					s1++;
					s1 %= __wrap;
				} while (__getlength(s1, s2, __wrap) > 0 && !(b.get(s1 % size)));
			} else b.set(i % size, false);
		}
	};
}
static unsigned long sdbm(uint8_t* str, int len) {
	unsigned long hash = 0;
	int c;
	for (int i = 0; i < len; i++) {
		c = str[i];
		hash = c + (hash << 6) + (hash << 16) - hash;
	}
	return hash;
}
namespace std
{
	template<>
	struct hash<CP::String>
	{
		size_t operator()(const ::CP::String& val) const {
			return sdbm((uint8_t*) val.data(), val.length());
		}
	};
}
#endif
