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
 * Testing.h
 *
 *  Created on: Sep 21, 2009
 *      Author: sumeet
 */

#ifndef TESTING_H_
#define TESTING_H_
#include "Test.h"
#include <iostream>

class Testing {
public:
	Testing();
	virtual ~Testing();
	void test1(string in);
	string test2();
	void test3(Test t);
	Test test4(string in);
};

#endif /* TESTING_H_ */
