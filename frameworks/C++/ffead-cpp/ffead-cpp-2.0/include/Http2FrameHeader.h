/*
 * Http2Frame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2FRAMEHEADER_H_
#define HTTP2FRAMEHEADER_H_
#include <iostream>
#include <bitset>
#include "string"
#include "stdint.h"
#include "vector"
using namespace std;

class Http2ErrorCode;

class Http2ErrorCode {
	uint32_t val;
	static void init()
	{
		if(PROTOCOL_ERROR.val!=1)
		{
			NO_ERROR.val = 0;
			PROTOCOL_ERROR.val = 1;
			INTERNAL_ERROR.val = 2;
			FLOW_CONTROL_ERROR.val = 3;
			SETTINGS_TIMEOUT.val = 4;
			STREAM_CLOSED.val = 5;
			FRAME_SIZE_ERROR.val = 6;
			REFUSED_STREAM.val = 7;
			CANCEL.val = 8;
			COMPRESSION_ERROR.val = 9;
			CONNECT_ERROR.val = 10;
			ENHANCE_YOUR_CALM.val = 11;
			INADEQUATE_SECURITY.val = 12;
			HTTP_1_1_REQUIRED.val = 13;
		}
	}
	Http2ErrorCode()
	{
		init();
	}
public:
	uint32_t get() const
	{
		return val;
	}
	static Http2ErrorCode NO_ERROR,
		PROTOCOL_ERROR,
		INTERNAL_ERROR,
		FLOW_CONTROL_ERROR,
		SETTINGS_TIMEOUT,
		STREAM_CLOSED,
		FRAME_SIZE_ERROR,
		REFUSED_STREAM,
		CANCEL,
		COMPRESSION_ERROR,
		CONNECT_ERROR,
		ENHANCE_YOUR_CALM,
		INADEQUATE_SECURITY,
		HTTP_1_1_REQUIRED;
	bool operator==(const Http2ErrorCode& other) const
	{
		init();
		if(this->val == other.val)
		{
			return true;
		}
		return false;
	}
	bool operator!=(const Http2ErrorCode& other) const
	{
		init();
		if(this->val != other.val)
		{
			return true;
		}
		return false;
	}
};

class Http2FrameHeader {
	int payloadLength;
	unsigned char type;
	bitset<8> flags;
	bool reserved;
	int streamIdentifier;
	friend class Http2Frame;
	friend class Http2Handler;
	friend class Http2StreamHandler;
	friend class Http2ContinuationFrame;
	friend class Http2DataFrame;
	friend class Http2HeadersFrame;
	friend class Http2PriorityFrame;
	friend class Http2ResetStreamFrame;
	friend class Http2SettingsFrame;
	friend class Http2PushPromiseFrame;
	friend class Http2PingFrame;
	friend class Http2GoAwayFrame;
	friend class Http2WindowUpdateFrame;
	friend class Http2AlternativeServicesFrame;
public:
	Http2FrameHeader();
	virtual ~Http2FrameHeader();
	bitset<8> getFlags() const;
	int getPayloadLength() const;
	bool isReserved() const;
	int getStreamIdentifier() const;
	unsigned char getType() const;
	bool isEndOfStream();
	bool isPadded();
	bool isEndHeaders();
	bool isPriority();
	bool isSettingsAck();
	bool isPingAck();

	bool isWsEndSegment();
};

#endif /* HTTP2FRAMEHEADER_H_ */
