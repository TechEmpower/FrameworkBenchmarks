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
 * TemplateHandler.h
 *
 *  Created on: Sep 12, 2009
 *      Author: sumeet
 */

#ifndef TEMPLATEHANDLER_H_
#define TEMPLATEHANDLER_H_
#include "string"
#include "map"
#include "StringUtil.h"
#include "GenericObject.h"
#include "HttpRequest.h"
#include <fstream>
using namespace std;
#include "TemplateEngine.h"

typedef map<string, GenericObject> Context;

class TemplateHandler {
public:
	virtual Context getContext(HttpRequest* request)=0;
};

#endif /* TEMPLATEHANDLER_H_ */
