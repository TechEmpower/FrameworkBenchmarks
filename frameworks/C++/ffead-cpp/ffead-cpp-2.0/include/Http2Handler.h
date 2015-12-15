/*
 * Http2Handler.h
 *
 *  Created on: 07-Dec-2014
 *      Author: sumeetc
 */

#ifndef HTTP2HANDLER_H_
#define HTTP2HANDLER_H_
#include "Http2ContinuationFrame.h"
#include "Http2DataFrame.h"
#include "Http2GoAwayFrame.h"
#include "Http2HeadersFrame.h"
#include "Http2PingFrame.h"
#include "Http2PriorityFrame.h"
#include "Http2PushPromiseFrame.h"
#include "Http2ResetStreamFrame.h"
#include "Http2SettingsFrame.h"
#include "Http2WindowUpdateFrame.h"
#include "Http2StreamHandler.h"
#include "Http2AlternativeServicesFrame.h"
#include "SocketUtil.h"
#include "CommonUtils.h"
#include "Http2ReadWriteUtil.h"
#include "SocketInterface.h"
#include "HttpRequest.h"
#include "HttpResponse.h"
#include "queue"

class Http2Handler : public Http2ReadWriteUtil, public SocketInterface {
	int highestStreamIdentifier;
	int highestPushPromiseStreamIdentifier;
	Http2HPACKContext context;
	//All the related entities for a PUSH_PROMISE frame
	map<string, vector<string> > relatedEntitiesForPP;
	map<int, Http2StreamHandler> streams;
	int senderFlowControlWindow;
	int receiverFlowControlWindow;
	map<int, bool> frameAcks;
	map<uint16_t, uint32_t> settings;
	int precedingstreamId;
	bool isConnInit;
	uint32_t maxDataFrameSize;
	string webpath;
	std::queue<Http2RequestResponseData> pushPromisedRequestQ;
	Http2Frame* readFrame();
	Http2Frame* readFrame(string data);
	Http2Frame* nextFrame();
	Http2Frame* getFrameByType(const string& data, Http2FrameHeader& header);
	string serialize(Http2Frame* frame);
	bool processFrame(Http2Frame* frame, void*& request);
public:
	void onOpen();
	void onClose();
	string getProtocol(void* context);
	int getTimeout();
	void* readRequest(void*& context, int& pending);
	bool writeResponse(void* req, void* res, void* context);
	bool writeHttpResponse(void* req, void* res, void* si);
	bool writeWebSocketResponse(void* req, void* res, void* si);
	void doIt();
	void addPushPromisedRequestToQ(const Http2RequestResponseData& ppdat);
	bool writeData(Http2Frame* frame);
	bool writePendingDataFrame(Http2RequestResponseData&);
	bool writeData(Http2RequestResponseData& data, Http2RequestResponseData& pendingSendData, int& streamFlowControlWindowS);
	vector<string> getRelatedEntitiesForPP(const string&);
	int getHighestPushPromiseStreamIdentifier();
	int updateSenderWindowSize(const int& windowSize);
	void updateMaxFrameSize(const uint32_t& val);
	Http2Handler(const bool& isServer, SocketUtil* sockUtil, const string& webpath);
	Http2Handler(const bool& isServer, SocketUtil* sockUtil, const string& webpath, const string& settingsFrameData);
	virtual ~Http2Handler();
	const string& getWebpath() const;
	string getMimeType(const string& ext);
};

#endif /* HTTP2HANDLER_H_ */
