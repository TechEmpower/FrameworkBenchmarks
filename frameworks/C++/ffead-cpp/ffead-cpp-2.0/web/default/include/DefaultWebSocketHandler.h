/*
 * DefaultWebSocketHandler.h
 *
 *  Created on: 04-Dec-2014
 *      Author: sumeetc
 */

#ifndef DEFAULTWEBSOCKETHANDLER_H_
#define DEFAULTWEBSOCKETHANDLER_H_
#include "Http11WebSocketDataFrame.h"

class DefaultWebSocketHandler {
public:
	DefaultWebSocketHandler();
	virtual ~DefaultWebSocketHandler();
	WebSocketData onOpen();
	WebSocketData onMessage(WebSocketData data);
	WebSocketData onClose();
};

#endif /* DEFAULTWEBSOCKETHANDLER_H_ */
