/*
 * Http2PriorityFrame.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2PRIORITYFRAME_H_
#define HTTP2PRIORITYFRAME_H_
#include "Http2Frame.h"

class Http2PriorityFrame : public Http2Frame {
	bool exclusive;
	int streamDependency;
	unsigned char weight;
	Http2PriorityFrame(string data, Http2FrameHeader& aheader);
	friend class Http2Handler;
public:
	Http2PriorityFrame();
	virtual ~Http2PriorityFrame();
	bool isExclusive() const;
	int getStreamDependency() const;
	unsigned char getWeight() const;
	string getFrameData();
};

#endif /* HTTP2PRIORITYFRAME_H_ */
