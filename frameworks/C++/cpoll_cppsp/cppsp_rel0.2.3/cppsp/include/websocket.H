#include <cpoll/cpoll.H>
#include <rgc.H>
using namespace CP;
using namespace RGC;
namespace cppsp
{
	struct WebSocketParser
	{
		struct ws_header1
		{
			//char flags:8;
			unsigned int opcode :4;
			bool rsv1 :1;
			bool rsv2 :1;
			bool rsv3 :1;
			bool fin :1;

			unsigned int payload_len :7;
			bool mask :1;
		}__attribute__((packed));
		struct ws_footer1
		{
			uint32_t masking_key;
		}__attribute__((packed));

		struct ws_header_extended16
		{
			uint16_t payload_len;
		}__attribute__((packed));

		struct ws_header_extended64
		{
			uint64_t payload_len;
		}__attribute__((packed));

		struct WSFrame
		{
			String data;
			char opcode;
			bool fin;
		};

		MemoryStream ms;
		int pos = 0;
		String beginPutData(int len) {
			if (ms.bufferSize - ms.bufferPos < len) ms.flushBuffer(len);
			return {(char*)ms.buffer + ms.bufferPos,ms.bufferSize-ms.bufferPos};
		}
		void endPutData(int len) {
			ms.bufferPos += len;
			ms.flush();
		}
		void skip(int length) {
			pos += length;
		}

		inline void unmask(String data, uint32_t key) {
			/*uint32_t* d = (uint32_t*) data.data();
			 int len = data.length() / sizeof(*d);
			 for (int i = 0; i < len; i++) {
			 d[i] ^= key;
			 }
			 uint8_t* tmp = (uint8_t*) (d + len);
			 uint8_t* tmp1 = (uint8_t*) &key;
			 int leftover = data.length() % sizeof(*d);
			 if (leftover > 0) tmp[0] ^= tmp1[0];
			 if (leftover > 1) tmp[1] ^= tmp1[1];
			 if (leftover > 2) tmp[2] ^= tmp1[2];
			 if (leftover > 3) tmp[3] ^= tmp1[3];*/
			uint8_t* k = (uint8_t*) &key;
			for (int i = 0; i < data.length(); i++) {
				data.d[i] = data.d[i] ^ k[i % sizeof(key)];
			}
		}
		bool process(WSFrame& out) {
			char* data = (char*) ms.data() + pos;
			int len = ms.length() - pos;
			int minLen = sizeof(ws_header1);
			if (len < minLen) return false;
			ws_header1* h1 = (ws_header1*) data;
			uint8_t pLen1 = h1->payload_len; // & ~(uint8_t) 128;
			//printf("pLen1 = %i\n", pLen1);
			int pLen2 = 0;
			if (pLen1 == 126) pLen2 = 2;
			if (pLen1 == 127) pLen2 = 8;
			minLen += pLen2;
			if (h1->mask) minLen += 4;
			if (len < minLen) return false;
			//printf("len = %i\n", len);
			//printf("minLen = %i\n", minLen);
			uint64_t payloadLen;

			switch (pLen1) {
				case 126:
				{
					ws_header_extended16* h2 = (ws_header_extended16*) (h1 + 1);
					payloadLen = ntohs(h2->payload_len);
					break;
				}
				case 127:
				{
					ws_header_extended64* h2 = (ws_header_extended64*) (h1 + 1);
					payloadLen = ntohll(h2->payload_len);
					break;
				}
				default:
					payloadLen = pLen1;
					break;
			}
			//printf("payloadLen = %lli\n", payloadLen);
			if (len < int(minLen + payloadLen)) return false;
			char* payload = data + minLen;
			out.data= {payload,(int)payloadLen};
			out.fin = h1->fin;
			out.opcode = h1->opcode;
			pos += minLen + (int) payloadLen;
			if (h1->mask) unmask( { payload, (int) payloadLen },
					((ws_footer1*) ((char*) (h1 + 1) + pLen2))->masking_key);
			return true;
		}

		//free up buffer space
		void reset() {
			if (pos > 0) {
				int shift = pos;
				if (ms.length() - shift > 0) memmove(ms.buffer, ms.buffer + shift, ms.length() - shift);
				ms.len -= shift;
				pos -= shift;
				ms.bufferPos = ms.len;
			}
		}

	};
	class FrameWriter
	{
	public:
		MemoryStream ms1, ms2;
		Ref<Stream> output;
		vector<String> queue;
		struct queueItem
		{
			int next; //is actually a pointer, but relative to the base of the array (MemoryStream)
			int len;
			char data[0];
		};
		int _first = -1, _last = -1, _count = 0;
		bool use_ms2 = false;
		bool _append;
		bool closed = false;
		bool writeQueued = false;
		inline MemoryStream& ms() {
			return use_ms2 ? ms2 : ms1;
		}
		inline queueItem& _item(int i) {
			return *(queueItem*) (ms().data() + i);
		}
		/**
		 Prepare for the insertion of a chunk into the queue;
		 @param append whether to append to the queue or insert at the beginning
		 @return the allocated buffer space; may be larger than the requested length
		 You must not call beginInsert again before calling endInsert.
		 */
		String beginInsert(int len, bool append = true) {
			_append = append;
			String tmp = ms().beginAppend(len + sizeof(queueItem));
			return tmp.subString(sizeof(queueItem));
		}
		/**
		 Complete the insertion of a chunk.
		 */
		void endInsert(int len) {
			//printf("endInsert: len=%i\n",len);
			int tmp = ms().length();
			ms().endAppend(len + sizeof(queueItem));
			if (_append) {
				_item(tmp).next = -1;
				if (_last >= 0) _item(_last).next = tmp;
				_last = tmp;
				if (_first < 0) _first = tmp;
			} else {
				_item(tmp).next = _first;
				_first = tmp;
				if (_last < 0) _last = tmp;
			}
			_item(tmp).len = len;
			++_count;
		}
		bool writing = false;
		void flush() {
			beginFlush();
		}
		void beginFlush() {
			if (writing) {
				writeQueued = true;
				return;
			}
			if (ms().length() <= 0 || _count <= 0) return;
			writing = true;
			int iovcnt = 0;
			iovec* iov = (iovec*) ms().beginAppend(sizeof(iovec) * _count).data();
			ms().endAppend(sizeof(iovec) * _count);

			for (int i = _first; i >= 0; i = _item(i).next) {
				iov[iovcnt++]= {_item(i).data,(size_t)_item(i).len};
				//printf("id=%i iovcnt=%i len=%i\n",i,iovcnt,_item(i).len);
			}
			use_ms2 = !use_ms2;
			_first = _last = -1;
			_count = 0;
			output->writevAll(iov, iovcnt, { &FrameWriter::_writevCB, this });
		}
		void _writevCB(int i) {
			writing = false;
			if (i <= 0) {
				closed = true;
				return;
			}
			if (writeQueued) {
				writeQueued = false;
				beginFlush();
			}
		}
	};
	String ws_beginWriteFrame(FrameWriter& fw, int len);
	void ws_endWriteFrame(FrameWriter& fw, String buf, int opcode);
	struct Page;
	struct Request;
	void ws_init(Page& p, CP::Callback cb);
	bool ws_iswebsocket(const cppsp::Request& req);
}
