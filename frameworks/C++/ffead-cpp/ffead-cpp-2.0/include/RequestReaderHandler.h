/*
 * RequestReaderHandler.h
 *
 *  Created on: 02-Jan-2015
 *      Author: sumeetc
 */

#ifndef REQUESTREADERHANDLER_H_
#define REQUESTREADERHANDLER_H_
#include "queue"
#include "string"
#include "SelEpolKqEvPrt.h"
#include "SocketInterface.h"
#include "Thread.h"
#include "Mutex.h"
#include "map"
#include "ReaderSwitchInterface.h"
#include "ServiceHandler.h"
#include "ConcurrentQueue.h"
using namespace std;

typedef SocketInterface* (*SocketInterfaceFactory) (SocketUtil*);

class RequestReaderHandler : public ReaderSwitchInterface {
	Mutex cMutex;
	ConcurrentQueue<SocketInterface*> pendingSocks;
	ConcurrentQueue<SocketInterface*> addToTimeoutSocks;
	ConcurrentQueue<SocketInterface*> remFromTimeoutSocks;
	ConcurrentQueue<SocketInterface*> timedoutSocks;
	ConcurrentQueue<SocketInterface*> readerSwitchedSocks;
	SelEpolKqEvPrt selector;
	bool run;
	bool isNotRegisteredListener;
	SOCKET listenerSock;
	ServiceHandler* shi;
	long siIdentifierSeries;
	SocketInterfaceFactory sf;
	map<int, SocketInterface*> connections;
	map<int, SocketInterface*> connectionsWithTimeouts;
	bool isActive();
	void addSf(SocketInterface* sf);
	static void* handleTimeouts(void* inp);
	static void* handle(void* inp);
public:
	void switchReaders(SocketInterface* prev, SocketInterface* next);
	void registerRead(SocketInterface* sd);
	void start();
	void stop();
	RequestReaderHandler(ServiceHandler* shi, const SOCKET& listenerSock= INVALID_SOCKET);
	void registerSocketInterfaceFactory(const SocketInterfaceFactory& f);
	virtual ~RequestReaderHandler();
};

#endif /* REQUESTREADERHANDLER_H_ */
