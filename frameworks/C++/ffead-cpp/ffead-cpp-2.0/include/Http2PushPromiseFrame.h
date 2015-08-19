/*
 * Http2PushPromiseFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2PUSHPROMISEFRAME_H_
#define HTTP2PUSHPROMISEFRAME_H_
#include "Http2Frame.h"

class Http2PushPromiseFrame : public Http2Frame {
	unsigned char padLength;
	bool reserved;
	int promisedStreamId;
	string headerBlockFragment;
	string padding;
	Http2PushPromiseFrame(string data, Http2FrameHeader& aheader);
	friend class Http2Handler;
	friend class Http2StreamHandler;
	map<string, string> headers;
public:
	Http2PushPromiseFrame();
	virtual ~Http2PushPromiseFrame();
	const string& getHeaderBlockFragment() const;
	const string& getPadding() const;
	unsigned char getPadLength() const;
	int getPromisedStreamId() const;
	bool isReserved() const;
	string getFrameData();
};

#endif /* HTTP2PUSHPROMISEFRAME_H_ */
