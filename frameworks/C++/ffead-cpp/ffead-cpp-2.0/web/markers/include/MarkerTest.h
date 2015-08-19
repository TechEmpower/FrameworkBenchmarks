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
 * MarkerTest.h
 *
 *  Created on: Sep 13, 2009
 *      Author: sumeet
 */

#ifndef MarkerTEST_H_
#define MarkerTEST_H_
#include "string"
using namespace std;

class MarkerTest {
	int id;
	string name;
public:
	MarkerTest();
	virtual ~MarkerTest();
    int getId() const;
    void setId(int);
    string getName() const;
    void setName(string);
    bool operator<(MarkerTest t) const;
};

#endif /* MarkerTEST_H_ */
