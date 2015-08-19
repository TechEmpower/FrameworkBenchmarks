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
 * RegexUtil.h
 *
 *  Created on: 14-Aug-2012
 *      Author: sumeetc
 */

#ifndef REGEXUTIL_H_
#define REGEXUTIL_H_
#include "AppDefines.h"
#if !defined(OS_MINGW)
#include <sys/types.h>
#endif
#include <regex.h>
#include "vector"
#include "map"
#include "string"
#include "StringUtil.h"
#include <iostream>
#include <stdlib.h>
using namespace std;

class RegexUtil {
	static map<string, regex_t> nlpatterns;
	static map<string, regex_t> patterns;
	static bool cacheRegexes;
	friend class ConfigurationHandler;
	static void getRegex(regex_t& regex, const string& pattern, const bool& matchNewLine);
public:
	static vector<string> findWithGroups(const string& text, const string& pattern, const int& groupCount, const bool& matchNewLine= false);
	static vector<string> findWithGroups(const string& text, const string& pattern, const bool& matchNewLine= false);
	static vector<string> search(const string& text, const string& pattern, const bool& matchNewLine= false);
	static void find(const string& text, const string& pattern, int &spos, int &epos, const bool& matchNewLine= false);
	static bool matches(const string& text, const string& pattern, const bool& matchNewLine= false);
	static int find(const string& text, const string& pattern, const bool& matchNewLine= false);
	static string replaceCopy(const string& text, const string& pattern, const string& with, const bool& matchNewLine= false);
	static bool replace(string& text, const string& pattern, const string& with, const bool& matchNewLine= false);
	static void flushCache();
};

#endif /* REGEXUTIL_H_ */
