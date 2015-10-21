/*
 * ProtocolServiceHandler.h
 *
 *  Created on: 07-Jan-2015
 *      Author: sumeetc
 */

#ifndef READERSWITCHINTERFACE_H_
#define READERSWITCHINTERFACE_H_
#include "SocketInterface.h"

class ReaderSwitchInterface {
protected:
	virtual void switchReaders(SocketInterface* prev, SocketInterface* next)=0;
	friend class ServiceHandler;
public:
	virtual ~ReaderSwitchInterface(){}
};



#endif /* READERSWITCHINTERFACE_H_ */
