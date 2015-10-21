/*
 * TeBkMessage.h
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#ifndef TEBKMESSAGE_H_
#define TEBKMESSAGE_H_
#include "string"

using namespace std;

class TeBkMessage {
	string message;
public:
	virtual ~TeBkMessage();
	const string& getMessage() const;
	void setMessage(const string& message);
};

#endif /* TEBKMESSAGE_H_ */
