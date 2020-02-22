/*
 * TeBkUmWorld.h
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#ifndef TeBkUmWORLD_H_
#define TeBkUmWORLD_H_
#include "string"


#pragma @Entity
#pragma @Table name="world"
class TeBkUmWorld {
	#pragma @Id dbf="_id"
	int id;
	#pragma @Column dbf="randomNumber"
	int randomNumber;
	#pragma @IgnoreSer
	#pragma @Column dbf="id"
	int anotherId;
public:
	TeBkUmWorld();
	virtual ~TeBkUmWorld();
	int getId() const;
	void setId(int id);
	int getRandomNumber() const;
	void setRandomNumber(int randomNumber);
	int getAnotherId() const;
	void setAnotherId(int anotherId);
};

#endif /* TeBkUmWORLD_H_ */
