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
 * XmlParseException.h
 *
 *  Created on: Sep 27, 2009
 *      Author: sumeet
 */

#ifndef XMLPARSEEXCEPTION_H_
#define XMLPARSEEXCEPTION_H_
#include "Exception.h"
#include "string"
using namespace std;

class XmlParseException : public Exception {
public:
	XmlParseException(const string&);
	virtual ~XmlParseException() throw();
};

#endif /* XMLPARSEEXCEPTION_H_ */
