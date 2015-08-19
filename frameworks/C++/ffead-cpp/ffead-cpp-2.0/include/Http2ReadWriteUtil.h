/*
 * Http2ReadWriteUtil.h
 *
 *  Created on: 13-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2READWRITEUTIL_H_
#define HTTP2READWRITEUTIL_H_
#include "CommonUtils.h"
#include "Http2Frame.h"
#include "HttpResponse.h"
#include "Http11WebSocketDataFrame.h"

class Http2RequestResponseData {
	map<string, string> preHeaders;
	map<string, string> postHeaders;
	string data;
	string url;
	bool isWebSocket;
	void* incompleteResponse;
	string headerBlock;
	int streamIdentifier;
	bool endStream;
	friend class Http2StreamHandler;
	friend class Http2Handler;
	friend class ServiceTask;
	void reset();
public:
	void updateContent();
	bool isDataPending();
	Http2RequestResponseData();
	virtual ~Http2RequestResponseData();
	const string& getData() const;
	const map<string, string>& getHeaders() const;
};

class Http2ReadWriteUtil {
public:
	Http2ReadWriteUtil();
	//virtual void close();
	virtual ~Http2ReadWriteUtil();
	virtual bool writePendingDataFrame(Http2RequestResponseData&)=0;
	//virtual bool writeData(Http2RequestResponseData& data, Http2RequestResponseData& pendingSendData, int& streamFlowControlWindowS)=0;
	virtual bool writeData(Http2Frame* data)=0;
	virtual vector<string> getRelatedEntitiesForPP(const string&)=0;
	virtual int getHighestPushPromiseStreamIdentifier()=0;
	virtual int updateSenderWindowSize(const int& windowSize)=0;
	virtual void addPushPromisedRequestToQ(const Http2RequestResponseData&)=0;
	virtual void updateMaxFrameSize(const uint32_t& val)=0;
	virtual string getMimeType(const string& ext)=0;
};

#endif /* HTTP2READWRITEUTIL_H_ */
