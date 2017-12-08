/*
 * TeBkMessage.cpp
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#include "TeBkMessage.h"

TeBkMessage::~TeBkMessage() {
	// TODO Auto-generated destructor stub
}

const std::string& TeBkMessage::getMessage() const {
	return message;
}

void TeBkMessage::setMessage(const std::string& message) {
	this->message = message;
}
