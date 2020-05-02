/*
 * TeBkUmWorld.cpp
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#include "TeBkUmWorld.h"

int TeBkUmWorld::getId() const {
	return id;
}

void TeBkUmWorld::setId(int id) {
	this->id = id;
}

int TeBkUmWorld::getRandomNumber() const {
	return randomNumber;
}

void TeBkUmWorld::setRandomNumber(int randomNumber) {
	this->randomNumber = randomNumber;
}

TeBkUmWorld::TeBkUmWorld() {
	id = 0;
	randomNumber = 0;
}

TeBkUmWorld::~TeBkUmWorld() {
	// TODO Auto-generated destructor stub
}

