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
 * MarkerDefTemp.h
 *
 *  Created on: Sep 12, 2009
 *      Author: sumeet
 */

#ifndef MarkerDefTemp_H_
#define MarkerDefTemp_H_
#include "TemplateHandler.h"
#include "vector"
#include "MarkerTest.h"
using namespace std;

#pragma @Template path="mark.tpe" file="mark.tpe"
class MarkerDefTemp : public TemplateHandler{
public:
	MarkerDefTemp();
	virtual ~MarkerDefTemp();
	Context getContext(HttpRequest* request);
};

#endif /* MarkerDefTemp_H_ */
