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
 * XmlParser.h
 *
 *  Created on: Sep 11, 2009
 *      Author: sumeet
 */

#ifndef XMLPARSER_H_
#define XMLPARSER_H_

#include <iostream>
#include "stdlib.h"
#include "Document.h"
#include <fstream>
#include "XmlParseException.h"
#include "CastUtil.h"
#include "LoggerFactory.h"
#include "StringUtil.h"

class XmlParser {
	public:
		XmlParser(const string&);
		virtual ~XmlParser();
		void parse(string, Document&);
		void readDocument(const string& filename, Document&);
	private:
		Logger logger;
		string mode;
		void readXML(string&, const string&, Element *);
};

#endif /* XMLPARSER_H_ */
