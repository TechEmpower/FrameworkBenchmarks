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
 * AfcUtil.h
 *
 *  Created on: Aug 27, 2009
 *      Author: sumeet
 */


#include "Reflection.h"
#include "Constants.h"
#include "CastUtil.h"
#include "iostream"
#include "fstream"
#include "sstream"
#include "map"
#include "vector"
#include "StringUtil.h"
#include "LoggerFactory.h"

using namespace std;
typedef vector<string> strVec;
typedef map<string, string> propMap;

#ifndef AFCUTIL_H_
#define AFCUTIL_H_

class ClassStructure;
class Reflection;

class AfcUtil {
public:
	AfcUtil();
	virtual ~AfcUtil();
	static string generateJsObjects(const strVec& obj, ClassStructure& classstruc, string &headers, string &objs, strVec pobj, const bool& isOpForSet, string& typrefs, const strVec& minfo, const string& app, const string& clspth, Reflection& ref);
	static string generateReadObjects(const string& type, const string& name, const bool& priv, const bool& ptr, const string& typ, const string& app, ClassStructure& classstruc, Reflection& ref);
	static string generateReadVectorObjects(const string& type, const string& name, const bool& priv, const bool& ptr, const string& typ, const string& conttype, const string& app, ClassStructure& classstruc, Reflection& ref);
	static string generateToJSONObjects(const string& type, const string& name, const bool& priv, const bool& end, string &retu, string &headers, string &objs, const string& typ, const bool& ptr, const string& app, ClassStructure& classstruc, Reflection& ref);
	static string generateToJSONVectorObjects(const string& type, const string& name, const bool& priv, string &retu, string &headers, string &objs, const string& typ, const bool& ptr, const string& stlcnttyp, const string& app, ClassStructure& classstruc, Reflection& ref);

	static string generateJsObjectsAll(map<string, ClassStructure>& allclsmap);
	static string generateJsObjects(strVec obj, const string& claz, strVec pobj, const strVec& minfo);

	static void writeTofile(const string&, const string&, const bool&);
	static string camelCased(const string&);
	static string reverseCamelCased(const string&);

	static string generateJsInterfacessAll(map<string, ClassStructure>& allclsmap, string &infjs, map<string, string>& ajintpthMap, strVec& afcd, Reflection& ref);
	static string generateJsInterfaces(const strVec& obj, ClassStructure& classstruc, const string& path, string &infjs, const string& appName, map<string, string>& ajintpthMap, Reflection& ref);
	static string updateAjaxInterface(const strVec& emp, ClassStructure& classstruc, const string& pars, const string& parswt, const string& types, const string& appName, Reflection& ref);


};

#endif /* AFCUTIL_H_ */
