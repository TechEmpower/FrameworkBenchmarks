/*
 * TeBkWorld.cpp
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#include "TeBkWorld.h"

int TeBkWorld::getId() const {
	return id;
}

void TeBkWorld::setId(int id) {
	this->id = id;
}

int TeBkWorld::getRandomNumber() const {
	return randomNumber;
}

void TeBkWorld::setRandomNumber(int randomNumber) {
	this->randomNumber = randomNumber;
}

TeBkWorld::TeBkWorld() {
	id = 0;
	randomNumber = 0;
	anotherId = 0;
}

TeBkWorld::~TeBkWorld() {
	// TODO Auto-generated destructor stub
}

int TeBkWorld::getAnotherId() const {
	return anotherId;
}

void TeBkWorld::setAnotherId(int anotherId) {
	this->anotherId = anotherId;
}

