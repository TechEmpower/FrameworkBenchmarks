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
 * JSONUtil.h
 *
 *  Created on: 06-Aug-2012
 *      Author: sumeetc
 */

#ifndef JSONUTIL_H_
#define JSONUTIL_H_
#include "string"
#include "JSONElement.h"
#include "StringUtil.h"
#include "CastUtil.h"
#include <stdio.h>
using namespace std;

class JSONUtil {
	static void array(string& json, JSONElement* element);
	static void object(string& json, JSONElement* element);
	static void arrayOrObject(string& json, JSONElement* element);
	static void readBalancedJSON(string& value, string& json, const bool& isarray, const size_t& obs);
	static void readJSON(string& json, const bool& isarray, JSONElement *par);
	static void validateSetValue(JSONElement* element, const string& value);
public:
	static void getDocument(const string& json, JSONElement& root);
	static string getDocumentStr(const JSONElement& doc);
	template <typename T> static vector<T>* toVectorP(const JSONElement& root)
	{
		vector<T>* vec = new vector<T>;
		for(int i=0;i<(int)root.getChildren().size();i++)
		{
			JSONElement* element = root.getChildren().at(i);
			if(element!=NULL)
				vec->push_back(CastUtil::lexical_cast<T>(element->getValue()));
		}
		return vec;
	}
};

#endif /* JSONUTIL_H_ */
