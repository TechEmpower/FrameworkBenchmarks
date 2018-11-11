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
	#pragma @Id dbf="_id"
	int id;
	#pragma @Column dbf="randomNumber"
	int randomNumber;
	#pragma @IgnoreSer
	#pragma @Column dbf="id"
	int anotherId;
public:
	TeBkWorld();
	virtual ~TeBkWorld();
	int getId() const;
	void setId(int id);
	int getRandomNumber() const;
	void setRandomNumber(int randomNumber);
	int getAnotherId() const;
	void setAnotherId(int anotherId);
};

#endif /* TEBKWORLD_H_ */
