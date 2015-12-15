/*
 * Http2Frame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2FRAME_H_
#define HTTP2FRAME_H_
#include "Http2FrameHeader.h"
#include "CommonUtils.h"

class Http2Frame {
protected:
	Http2FrameHeader header;
	friend class Http2Handler;
	friend class Http2StreamHandler;
public:
	Http2FrameHeader& getHeader();
	virtual ~Http2Frame();
	virtual string getFrameData()=0;
};

#endif /* HTTP2FRAME_H_ */
