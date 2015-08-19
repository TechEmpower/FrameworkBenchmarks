/*
	Copyright 2009-2012, Sumeet Chhetri

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
 * MarkerIOFilter.h
 *
 *  Created on: Apr 7, 2011
 *      Author: sumeet
 */

#ifndef MarkerRESTCONTROLLER_H_
#define MarkerRESTCONTROLLER_H_

#include <math.h>
#include <iostream>
#include "vector"
#include <fstream>

using namespace std;

#pragma @RestController path="/markRest"
#pragma @Secure role="ROLE_USER" providerName="markerProvider"
class MarkerRestController {
public:
	MarkerRestController();
	virtual ~MarkerRestController();
	#pragma @GET path="/" statusCode="200"
	int addNumbers(
			#pragma @QueryParam name="a"
			int,
			#pragma @QueryParam name="b"
			int);
};

#endif /* MarkerRESTCONTROLLER_H_ */
