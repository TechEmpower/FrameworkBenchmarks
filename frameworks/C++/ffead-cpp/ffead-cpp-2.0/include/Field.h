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
 * Field.h
 *
 *  Created on: Oct 10, 2009
 *      Author: sumeet
 */

#ifndef FIELD_H_
#define FIELD_H_
#include "string"
#include "vector"
using namespace std;

class Field {
	string refName;
	string accessSpecifier;
	string fieldName;
	string type;
	string initVal;
public:
	Field();
	virtual ~Field();
	const string& getAccessSpecifier() const;
    void setAccessSpecifier(const string&);
    const string& getFieldName() const;
    void setFieldName(const string&);
    const string& getType() const;
    void setType(const string&);
    void clear();
	const string& getRefName() const;
	void setRefName(const string& refName);
};

#endif /* FIELD_H_ */
