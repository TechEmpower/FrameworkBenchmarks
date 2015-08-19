/*
 * Http2WindowUpdateFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2WINDOWUPDATEFRAME_H_
#define HTTP2WINDOWUPDATEFRAME_H_
#include "Http2Frame.h"

class Http2WindowUpdateFrame : public Http2Frame {
	bool reserved;
	int windowSizeIncrement;
	Http2WindowUpdateFrame(string data, Http2FrameHeader& aheader);
	friend class Http2Handler;
	friend class Http2StreamHandler;
public:
	Http2WindowUpdateFrame();
	virtual ~Http2WindowUpdateFrame();
	bool isReserved() const;
	int getWindowSizeIncrement() const;
	string getFrameData();
};

#endif /* HTTP2WINDOWUPDATEFRAME_H_ */
