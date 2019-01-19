/*
 * TeBkFortune.h
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#ifndef TEBKFORTUNE_H_
#define TEBKFORTUNE_H_
#include "string"



#pragma @Entity
#pragma @Table name="fortune"
class TeBkFortune {
	#pragma @Id dbf="id"
	int id;
	#pragma @Column dbf="message"
	std::string message;
public:
	TeBkFortune();
	virtual ~TeBkFortune();
	int getId() const;
	void setId(int id);
	const std::string& getMessage() const;
	void setMessage(const std::string& message);
	bool operator < (const TeBkFortune& other) const;
};

#endif /* TEBKFORTUNE_H_ */
