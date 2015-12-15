/*
 * ServiceHandler.h
 *
 *  Created on: 03-Jan-2015
 *      Author: sumeetc
 */

#ifndef SERVICEHANDLER_H_
#define SERVICEHANDLER_H_
#include "Thread.h"
#include "Mutex.h"
#include "queue"
#include "ReaderSwitchInterface.h"
#include "Http11Handler.h"
#include "Http2Handler.h"
#include "Http11WebSocketHandler.h"
#include "ThreadPool.h"
#include "Thread.h"
#include "ConcurrentQueue.h"
#include "ConcurrentMap.h"

class ServiceHandler;

class HandlerRequest {
	void* request;
	SocketInterface* sif;
	void* context;
	bool sentResponse;
	string protocol;
	ServiceHandler* sh;
	ReaderSwitchInterface* switchReaderIntf;
	friend class ServiceHandler;
	HandlerRequest();
public:
	SocketUtil* getSocketUtil();
	void setSentResponse();
	virtual ~HandlerRequest();
	void* getContext();
	const string& getProtocol() const;
	void* getRequest();
	bool isSentResponse() const;
	SocketInterface* getSif();
	ReaderSwitchInterface* getSwitchReaderIntf();
};

class ServiceHandler {
	Mutex mutex;
	ConcurrentQueue<SocketInterface*> tbcSifQ;
	ConcurrentMap<long, int> requestNumMap;
	bool run;
	bool isThreadPerRequest;
	int poolSize;
	ThreadPool pool;
	bool addOpenRequest(SocketInterface* si);
	void addCloseRequest(SocketInterface* si);
	bool isAvailable(SocketInterface* si);
	void registerRequest(void* request, SocketInterface* sif, void* context, ReaderSwitchInterface* switchReaderIntf);
	bool isActive();
	static void* taskService(void* inp);
	static void* cleanSifs(void* inp);
	void flagDone(SocketInterface* si);
	friend class RequestReaderHandler;
	friend class HandlerRequest;
protected:
	void submitTask(Task* task);
	virtual void service(HandlerRequest* req)=0;
public:
	void switchReaders(HandlerRequest* hr, SocketInterface* next);
	void start();
	void stop();
	ServiceHandler(const int& poolSize);
	virtual ~ServiceHandler();
};

#endif /* SERVICEHANDLER_H_ */
