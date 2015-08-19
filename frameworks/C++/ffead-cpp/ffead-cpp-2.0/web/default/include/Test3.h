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
 * Test3.h
 *
 *  Created on: Apr 17, 2010
 *      Author: sumeet
 */

#ifndef TEST3_H_
#define TEST3_H_
#include "Test.h"
#include "vector"

class Test3 {
	int id;
	int test_id;
	vector<Test> tests;
public:
	Test3();
	virtual ~Test3();
    int getId() const;
    void setId(int id);
    vector<Test> getTests() const;
    void setTests(vector<Test> tests);
    int getTest_id() const;
    void setTest_id(int test_id);
};

#endif /* TEST3_H_ */
