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
 * Filter.h
 *
 *  Created on: Apr 7, 2011
 *      Author: sumeet
 */

#ifndef FILTER_H_
#define FILTER_H_
#include "HttpResponse.h"
#include "HttpRequest.h"

class Filter {
public:
	virtual void doInputFilter(HttpRequest *req){}
	virtual bool doHandle(HttpRequest *req, HttpResponse* res){return true;}
	virtual void doOutputFilter(HttpResponse *res){}
};

#endif /* FILTER_H_ */
