/*
 * TeBkMessage.h
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#ifndef TEBKMESSAGE_H_
#define TEBKMESSAGE_H_
#include "string"



class TeBkMessage {
	std::string message;
public:
	virtual ~TeBkMessage();
	const std::string& getMessage() const;
	void setMessage(const std::string& message);
};

#endif /* TEBKMESSAGE_H_ */
