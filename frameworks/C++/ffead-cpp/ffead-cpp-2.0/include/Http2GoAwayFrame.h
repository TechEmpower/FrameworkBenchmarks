/*
 * Http2GoAwayFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2GOAWAYFRAME_H_
#define HTTP2GOAWAYFRAME_H_
#include "Http2Frame.h"

class Http2GoAwayFrame : public Http2Frame {
	bool reserved;
	int lastStreamId;
	uint32_t errorCode;
	string additionalDebugData;
	Http2GoAwayFrame(string data, Http2FrameHeader& aheader);
	friend class Http2Handler;
	friend class Http2StreamHandler;
public:
	Http2GoAwayFrame();
	virtual ~Http2GoAwayFrame();
	const string& getAdditionalDebugData() const;
	uint32_t getErrorCode() const;
	int getLastStreamId() const;
	bool isReserved() const;
	string getFrameData();
};

#endif /* HTTP2GOAWAYFRAME_H_ */
