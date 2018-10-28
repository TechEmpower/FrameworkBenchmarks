/*
 * TeBkWorld.h
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#ifndef TEBKWORLD_H_
#define TEBKWORLD_H_
#include "string"


#pragma @Entity
#pragma @Table name="world"
class TeBkWorld {
	#pragma @Id dbf="id"
	int id;
	#pragma @Column dbf="randomNumber"
	int randomNumber;
public:
	TeBkWorld();
	virtual ~TeBkWorld();
	int getId() const;
	void setId(int id);
	int getRandomNumber() const;
	void setRandomNumber(int randomNumber);
};

#endif /* TEBKWORLD_H_ */
