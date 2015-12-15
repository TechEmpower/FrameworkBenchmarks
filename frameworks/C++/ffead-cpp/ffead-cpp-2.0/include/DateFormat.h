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
 * DateFormat.h
 *
 *  Created on: Jun 4, 2010
 *      Author: sumeet
 */

#ifndef DATEFORMAT_H_
#define DATEFORMAT_H_
#include "string"
#include "Date.h"
#include "vector"
#include <iostream>

using namespace std;

class DateFormat {
	string formatspec;
	string appendZero(const int& value);
public:
	DateFormat();
	virtual ~DateFormat();
	DateFormat(const string&);
	string format(const Date&);
	Date* parse(string);
    const string& getFormatspec() const;
    void setFormatspec(const string& formatspec);
};

#endif /* DATEFORMAT_H_ */
