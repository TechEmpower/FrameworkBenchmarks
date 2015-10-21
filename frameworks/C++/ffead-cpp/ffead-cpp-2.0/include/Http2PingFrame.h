/*
 * Http2PingFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2PINGFRAME_H_
#define HTTP2PINGFRAME_H_
#include "Http2Frame.h"

class Http2PingFrame : public Http2Frame {
	uint64_t opaqueData;
	Http2PingFrame(const string& data, Http2FrameHeader& aheader);
	friend class Http2Handler;
	friend class Http2StreamHandler;
public:
	Http2PingFrame();
	virtual ~Http2PingFrame();
	uint64_t getOpqueData() const;
	string getFrameData();
};

#endif /* HTTP2PINGFRAME_H_ */
