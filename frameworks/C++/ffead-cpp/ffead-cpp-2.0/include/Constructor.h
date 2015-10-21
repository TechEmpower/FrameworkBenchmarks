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
 * Constructor.h
 *
 *  Created on: Mar 30, 2010
 *      Author: sumeet
 */

#ifndef CONSTRUCTOR_H_
#define CONSTRUCTOR_H_
#include "string"
#include "vector"
using namespace std;
class Constructor {
	string refName;
	string name;
	vector<string> argumentTypes;
public:
	Constructor();
	virtual ~Constructor();
	vector<string> getArgumentTypes() const;
	void setArgumentTypes(const vector<string>&);
	int getArgNum() const;
    string getName() const;
    void setName(const string& name);
    void clear();
	const string& getRefName() const;
	void setRefName(const string& refName);
};

#endif /* CONSTRUCTOR_H_ */
