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
 * Test.h
 *
 *  Created on: Sep 13, 2009
 *      Author: sumeet
 */

#ifndef TEST_H_
#define TEST_H_
#include "string"
using namespace std;

class Test {
	int id;
	string name;
public:
	Test();
	virtual ~Test();
    int getId() const;
    void setId(int);
    string getName() const;
    void setName(string);
    bool operator<(Test t) const;
};

#endif /* TEST_H_ */
