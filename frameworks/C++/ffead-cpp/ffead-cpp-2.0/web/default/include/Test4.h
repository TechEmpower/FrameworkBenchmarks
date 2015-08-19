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
 * Test4.h
 *
 *  Created on: Jun 9, 2010
 *      Author: sumeet
 */

#ifndef TEST4_H_
#define TEST4_H_
#include "Date.h"
#include "BinaryData.h"

class Test4 {
	Date date;
	Date datt;
	Date dattm;
	BinaryData binar;
public:
	Test4();
	virtual ~Test4();
	Date getDate() const;
    void setDate(Date date);
    Date getDatt() const;
    void setDatt(Date datt);
    Date getDattm() const;
    void setDattm(Date dattm);
    BinaryData getBinar() const;
    void setBinar(BinaryData binar);
};

#endif /* TEST4_H_ */
