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
 * DCPGenerator.h
 *
 *  Created on: Sep 5, 2009
 *      Author: sumeet
 */

#ifndef DCPGENERATOR_H_
#define DCPGENERATOR_H_
#include "PropFileReader.h"
#include "ConfigurationData.h"

using namespace std;

class DCPGenerator {
public:
	DCPGenerator();
	virtual ~DCPGenerator();
	static string generateDCP(const string&, string &, string &, const string&);
	static string generateDCPAll();
};

#endif /* DCPGENERATOR_H_ */
