/*
 * Http2ContinuationFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2CONTINUATIONFRAME_H_
#define HTTP2CONTINUATIONFRAME_H_
#include "Http2Frame.h"

class Http2ContinuationFrame : public Http2Frame {
	string headerBlockFragment;
	Http2ContinuationFrame(const string& data, Http2FrameHeader& aheader);
	friend class Http2Handler;
	friend class Http2StreamHandler;
public:
	Http2ContinuationFrame();
	virtual ~Http2ContinuationFrame();
	const string& getHeaderBlockFragment() const;
	string getFrameData();
};

#endif /* HTTP2CONTINUATIONFRAME_H_ */
