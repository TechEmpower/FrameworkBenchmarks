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
