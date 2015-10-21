/*
 * HttpServiceHandler.h
 *
 *  Created on: 07-Jan-2015
 *      Author: sumeetc
 */

#ifndef HTTPSERVICEHANDLER_H_
#define HTTPSERVICEHANDLER_H_
#include "ServiceHandler.h"
#include "Task.h"
#include "CommonUtils.h"

class HttpServiceHandler;

class HttpServiceTask : public Task {
	HandlerRequest* handlerRequest;
	HttpServiceHandler* service;
	HttpServiceTask(HandlerRequest* handlerRequest, HttpServiceHandler* service);
	void run();
	friend class HttpServiceHandler;
public:
	virtual ~HttpServiceTask();
	HttpServiceTask();
	virtual void handle(HttpRequest* request, HttpResponse* response)=0;
	virtual void handleWebsockOpen(const string& url)=0;
	virtual void handleWebsockClose(const string& url)=0;
	virtual void handleWebsockMessage(const string& url, WebSocketData* request, WebSocketData* response)=0;
};

typedef HttpServiceTask* (*HttpServiceTaskFactory) ();

class HttpServiceHandler : public ServiceHandler {
	string cntEncoding;
	HttpServiceTaskFactory f;
	friend class HttpServiceTask;
	void service(HandlerRequest* handlerRequest);
public:
	HttpServiceHandler(const string& cntEncoding, const HttpServiceTaskFactory& f, const int& poolSize);
	virtual ~HttpServiceHandler();
};

#endif /* HTTPSERVICEHANDLER_H_ */
