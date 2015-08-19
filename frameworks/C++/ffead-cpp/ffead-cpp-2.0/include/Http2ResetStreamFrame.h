/*
 * Http2ResetStreamFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2RESETSTREAMFRAME_H_
#define HTTP2RESETSTREAMFRAME_H_
#include "Http2Frame.h"

class Http2ResetStreamFrame : public Http2Frame {
	unsigned int errorCode;
	Http2ResetStreamFrame(const string& data, Http2FrameHeader& aheader);
	friend class Http2Handler;
public:
	Http2ResetStreamFrame();
	virtual ~Http2ResetStreamFrame();
	unsigned int getErrorCode() const;
	string getFrameData();
};

#endif /* HTTP2RESETSTREAMFRAME_H_ */
