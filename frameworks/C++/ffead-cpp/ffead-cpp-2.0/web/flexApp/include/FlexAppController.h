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
 * FlexAppController.h
 *
 *  Created on: Aug 20, 2009
 *      Author: sumeet
 */

#ifndef FLEXAPPCONTROLLER_H_
#define FLEXAPPCONTROLLER_H_
#include <iostream>
#include "Controller.h"

class FlexAppController : public Controller{
public:
	FlexAppController();
	virtual ~FlexAppController();
	bool service(HttpRequest* req, HttpResponse* res);
};

#endif /* FLEXAPPCONTROLLER_H_ */
