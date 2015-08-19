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
 * PropFileReader.h
 *
 *  Created on: Aug 18, 2009
 *      Author: sumeet
 */

#ifndef PROPFILEREADER_H_
#define PROPFILEREADER_H_
#include "fstream"
#include "sstream"
#include "map"
#include "vector"
#include "StringUtil.h"

using namespace std;
typedef vector<string> strVec;
typedef map<string, string> propMap;
typedef map<string, vector<string> > propMultiMap;
class PropFileReader {
public:
	PropFileReader();
	PropFileReader(const bool&);
	virtual ~PropFileReader();
	propMap getProperties(const string&);
	propMultiMap getPropertiesMultiMap(const string&);
private:
	bool mergeSimProps;
};

#endif /* PROPFILEREADER_H_ */
