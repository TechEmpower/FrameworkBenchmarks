/*
 * TeBkFortune.h
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#ifndef TEBKFORTUNE_H_
#define TEBKFORTUNE_H_
#include "string"

using namespace std;

#pragma @Entity
#pragma @Table name="Fortune"
class TeBkFortune {
	#pragma @Id dbf="_id"
	int id;
	#pragma @Column dbf="message"
	string message;
public:
	virtual ~TeBkFortune();
	int getId() const;
	void setId(int id);
	const string& getMessage() const;
	void setMessage(const string& message);
	bool operator < (const TeBkFortune& other) const;
};

#endif /* TEBKFORTUNE_H_ */
