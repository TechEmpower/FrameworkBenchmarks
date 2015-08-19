/*
 * Http2HeadersFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2HEADERSFRAME_H_
#define HTTP2HEADERSFRAME_H_
#include "Http2Frame.h"

class Http2HeadersFrame : public Http2Frame {
	unsigned char padLength;
	bool exclusive;
	int streamDependency;
	unsigned char weight;
	string headerBlockFragment;
	string padding;
	Http2HeadersFrame(string data, Http2FrameHeader& header);
	friend class Http2Handler;
	friend class Http2StreamHandler;
public:
	Http2HeadersFrame();
	virtual ~Http2HeadersFrame();
	bool isExclusive() const;
	const string& getHeaderBlockFragment() const;
	void setHeaderBlockFragment(const string& headerBlockFragment);
	const string& getPadding() const;
	unsigned char getPadLength() const;
	int getStreamDependency() const;
	unsigned char getWeight() const;
	string getFrameData();
};

#endif /* HTTP2HEADERSFRAME_H_ */
