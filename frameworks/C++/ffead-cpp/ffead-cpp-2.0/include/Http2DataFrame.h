/*
 * Http2DataFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2DATAFRAME_H_
#define HTTP2DATAFRAME_H_
#include "Http2Frame.h"

class Http2DataFrame : public Http2Frame {
	unsigned char padLength;
	string data;
	string padding;
	Http2DataFrame(const string& data, Http2FrameHeader& aheader);
	friend class Http2Handler;
	friend class Http2StreamHandler;
public:
	Http2DataFrame();
	virtual ~Http2DataFrame();
	const string& getData() const;
	const string& getPadding() const;
	unsigned char getPadLength() const;
	string getFrameData();
};

#endif /* HTTP2DATAFRAME_H_ */
