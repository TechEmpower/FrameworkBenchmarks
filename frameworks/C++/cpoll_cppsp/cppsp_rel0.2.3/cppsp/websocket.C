/*
 * websocket.C
 *
 *  Created on: Jun 1, 2013
 *      Author: xaxaxa
 */

#include "include/websocket.H"
#include "include/page.H"
#include <cryptopp/cryptlib.h>
#include <cryptopp/sha.h>
#include <cryptopp/filters.h>
#include <cryptopp/base64.h>
using namespace CryptoPP;
using namespace CP;
using namespace std;

namespace cppsp
{
	static uint64_t htonll(uint64_t value) {
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
	//len must be known in advance; you can not pass a subString of the returned buffer to ws_endWriteFrame()
	String ws_beginWriteFrame(FrameWriter& fw, int len) {
		int hdrlen = sizeof(WebSocketParser::ws_header1);
		if (len > 125 && len <= 0xFFFF) hdrlen += sizeof(WebSocketParser::ws_header_extended16);
		if (len > 0xFFFF) hdrlen += sizeof(WebSocketParser::ws_header_extended64);
		String buf = fw.beginInsert(hdrlen + len);
		return buf.subString(hdrlen, len);
	}
	void ws_endWriteFrame(FrameWriter& fw, String buf, int opcode) {
		int hdrlen = sizeof(WebSocketParser::ws_header1);
		if (buf.length() > 125 && buf.length() <= 0xFFFF) hdrlen +=
				sizeof(WebSocketParser::ws_header_extended16);
		if (buf.length() > 0xFFFF) hdrlen += sizeof(WebSocketParser::ws_header_extended64);

		WebSocketParser::ws_header1* h1 = ((WebSocketParser::ws_header1*) (buf.data() - hdrlen));
		memset(h1, 0, sizeof(*h1));
		h1->fin = true;
		h1->mask = false;
		h1->opcode = opcode;
		if (buf.length() > 125 && buf.length() <= 0xFFFF) {
			h1->payload_len = 126;
			WebSocketParser::ws_header_extended16* h2 = (WebSocketParser::ws_header_extended16*) (h1
					+ 1);
			h2->payload_len = htons((uint16_t) buf.length());
		} else if (buf.length() > 0xFFFF) {
			h1->payload_len = 127;
			WebSocketParser::ws_header_extended64* h2 = (WebSocketParser::ws_header_extended64*) (h1
					+ 1);
			h2->payload_len = htonll((uint64_t) buf.length());
		} else {
			h1->payload_len = (char) buf.length();
		}
		fw.endInsert(hdrlen + buf.length());
	}
	void ws_init(Page& p, CP::Callback cb) {
		p.response->statusCode = 101;
		p.response->statusName = "Switching Protocols";
		p.response->headers["Connection"] = "Upgrade";
		p.response->headers["Upgrade"] = "WebSocket";
		//response->headers["Sec-WebSocket-Protocol"]="chat";
		String s = concat(*p.sp, p.request->headers["Sec-WebSocket-Key"],
				"258EAFA5-E914-47DA-95CA-C5AB0DC85B11");

		SHA1 sha1;
		byte tmp[sha1.DigestSize()];
		sha1.CalculateDigest(tmp, (const byte*) s.data(), s.length());

		string encoded;
		StringSource src(tmp, sizeof(tmp), true, new Base64Encoder(new StringSink(encoded), false));
		//printf("Sec-WebSocket-Accept: %s\n",encoded.c_str());
		p.response->headers["Sec-WebSocket-Accept"] = p.sp->addString(encoded);
		p.response->serializeHeaders(p.response->output);
		p.response->output.flush();
		p.response->outputStream->write(p.response->buffer, cb);
	}
	bool ws_iswebsocket(const Request& req) {
		return (ci_compare(req.headers["Connection"], "Upgrade") == 0
				&& ci_compare(req.headers["Upgrade"], "websocket") == 0);
	}
}

