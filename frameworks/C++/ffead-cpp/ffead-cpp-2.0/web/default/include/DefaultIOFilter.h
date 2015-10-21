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
 * DefaultIOFilter.h
 *
 *  Created on: Apr 7, 2011
 *      Author: sumeet
 */

#ifndef DEFAULTIOFILTER_H_
#define DEFAULTIOFILTER_H_

#include "Filter.h"
#include <iostream>

class DefaultIOFilter: public Filter {
public:
	DefaultIOFilter();
	virtual ~DefaultIOFilter();
	void doInputFilter(HttpRequest *req);
	void doOutputFilter(HttpResponse *res);
	bool doHandle(HttpRequest *req, HttpResponse* res);
};

#endif /* DEFAULTIOFILTER_H_ */
