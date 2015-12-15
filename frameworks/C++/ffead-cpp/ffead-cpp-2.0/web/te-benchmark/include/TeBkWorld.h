/*
 * TeBkWorld.h
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#ifndef TEBKWORLD_H_
#define TEBKWORLD_H_
#include "string"
using namespace std;

#pragma @Entity
#pragma @Table name="World"
class TeBkWorld {
	#pragma @Id dbf="_id"
	int id;
	#pragma @Column dbf="randomNumber"
	int randomNumber;
public:
	virtual ~TeBkWorld();
	int getId() const;
	void setId(int id);
	int getRandomNumber() const;
	void setRandomNumber(int randomNumber);
};

#endif /* TEBKWORLD_H_ */
