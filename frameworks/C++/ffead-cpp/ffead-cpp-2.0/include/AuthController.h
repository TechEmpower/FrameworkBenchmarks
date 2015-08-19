/*
	Copyright 2009-2012, Sumeet Chhetri

    Licensed under the Apache License, Version 2.0 (const the& "License");
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
 * AuthController.h
 *
 *  Created on: Nov 23, 2010
 *      Author: sumeet
 */

#ifndef AUTHCONTROLLER_H_
#define AUTHCONTROLLER_H_
#include "string"
#include "map"
#include "HttpRequest.h"
#include "HttpResponse.h"
using namespace std;
class AuthController {
public:
	AuthController();
	virtual ~AuthController();
	virtual bool authenticate(const string& user, const string& password)=0;
	virtual string getUserRole(const string& username)=0;
	virtual bool isInitialized()=0;
};

#endif /* AUTHCONTROLLER_H_ */
