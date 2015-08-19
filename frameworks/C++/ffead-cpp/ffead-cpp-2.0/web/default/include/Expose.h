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
 * Expose.h
 *
 *  Created on: Aug 28, 2009
 *      Author: sumeet
 */

#ifndef EXPOSE_H_
#define EXPOSE_H_
#include "PropFileReader.h"
#include "YObject.h"

class Expose {
public:
	Expose();
	virtual ~Expose();
	YObject sayHello(string,int,float);
	string sayHello1(string,int,float);
	YObject sayHello2(YObject,int,float);
};

#endif /* EXPOSE_H_ */
