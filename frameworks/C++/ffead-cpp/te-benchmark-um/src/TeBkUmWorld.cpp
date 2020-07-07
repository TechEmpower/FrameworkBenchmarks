/*
	Copyright 2009-2020, Sumeet Chhetri

    Licensed under the Apache License, Version 2.0 (the "License");
    you may not use this file except in compliance with the License.
    You may obtain a copy of the License at

        http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software
    distributed under the License is distributed on an "AS IS" BASIS,
    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and
    limitations under the License.
*/
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
	anotherId = 0;
}

TeBkUmWorld::~TeBkUmWorld() {
	// TODO Auto-generated destructor stub
}

int TeBkUmWorld::getAnotherId() const {
	return anotherId;
}

void TeBkUmWorld::setAnotherId(int anotherId) {
	this->anotherId = anotherId;
}

