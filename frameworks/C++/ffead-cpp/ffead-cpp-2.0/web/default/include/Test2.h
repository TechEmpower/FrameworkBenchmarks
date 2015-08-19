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
 * Test2.h
 *
 *  Created on: Apr 13, 2010
 *      Author: sumeet
 */

#ifndef TEST2_H_
#define TEST2_H_
#include "Test.h"

class Test2 {
	int id;
	int test_id;
	Test test;
public:
	Test2();
	virtual ~Test2();
    int getId() const;
    void setId(int id);;
    int getTest_id() const;
    void setTest_id(int test_id);
    Test getTest() const;
    void setTest(Test test);
};

#endif /* TEST2_H_ */
