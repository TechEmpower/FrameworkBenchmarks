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
 * StringUtil.h
 *
 *  Created on: Aug 5, 2012
 *      Author: Sumeet
 */

#ifndef STRINGUTIL_H_
#define STRINGUTIL_H_
#include "string"
#include <algorithm>
#include <sstream>
#include <iterator>
#include "vector"
#include <stdio.h>
#include "cstring"
using namespace std;

class StringUtil {
	static string whitespaces;
public:
	static void eraseAll(string &str, const string& ths);
	static void capitalized(string &str);
	static string capitalizedCopy(const string &str);
	static void camelCased(string& str, const string& delim);
	static string camelCasedCopy(const string& str, const string& delim);
	static void toUpper(string &str);
	static string toUpperCopy(const string& str);
	static void toLower(string& str);
	static string toLowerCopy(const string& str);
	static void replaceFirst(string &str, const string& ths, const string& with);
	static string replaceFirstCopy(const string &str, const string& ths, const string& with);
	static void replaceLast(string &str, const string& ths, const string& with);
	static string replaceLastCopy(const string &str, const string& ths, const string& with);
	static void replaceAll(string &str, const string& ths, const string& with);
	static string replaceAllCopy(const string &str, const string& ths, const string& with);
	static vector<string> split(const string& input, const vector<string>& delimiters);
	static void split(vector<string> &output, const string& input, const vector<string>& delimiters);
	template < class ContainerT >
	static void split(ContainerT& tokens, const std::string& str, const std::string& delimiters, const bool& trimEmpty= false)
	{
		typedef ContainerT Base;
		typedef typename Base::value_type Vt;
		typedef typename Vt::size_type St;

		std::string::size_type pos, lastPos = 0;

		while(true)
		{
			pos = str.find_first_of(delimiters, lastPos);
			if(pos == std::string::npos)
			{
				pos = str.length();

				if(pos != lastPos || !trimEmpty)
					tokens.push_back(Vt(str.data()+lastPos,
							(St)pos-lastPos ));

				break;
			}
			else
			{
				if(pos != lastPos || !trimEmpty)
					tokens.push_back(Vt(str.data()+lastPos,
							(St)pos-lastPos ));
			}

			lastPos = pos + 1;
		}
	}
	template < class ContainerT >
	static ContainerT splitAndReturn(const std::string& str, const std::string& delimiters, const bool& trimEmpty= false)
	{
		typedef ContainerT Base;
		typedef typename Base::value_type Vt;
		typedef typename Vt::size_type St;

		ContainerT tokens;
		std::string::size_type pos, lastPos = 0;

		while(true)
		{
			pos = str.find_first_of(delimiters, lastPos);
			if(pos == std::string::npos)
			{
				pos = str.length();

				if(pos != lastPos || !trimEmpty)
					tokens.push_back(Vt(str.data()+lastPos,
							(St)pos-lastPos ));

				break;
			}
			else
			{
				if(pos != lastPos || !trimEmpty)
					tokens.push_back(Vt(str.data()+lastPos,
							(St)pos-lastPos ));
			}

			lastPos = pos + 1;
		}
		return tokens;
	}
	static int countOccurrences(const string& input, const string& delimiter);
	static void trim(string& str);
	static string trimCopy(const string &str);
	static string toHEX(const int&);
	static string toHEX(const unsigned int&);
	static string toHEX(const long&);
	static string toHEX(const unsigned long&);
	static string toHEX(const long long&);
	static string toHEX(const unsigned long long&);
	static string toOCTAL(const int&);
	static string toOCTAL(const unsigned int&);
	static string toOCTAL(const long&);
	static string toOCTAL(const unsigned long&);
	static string toOCTAL(const long long&);
	static string toOCTAL(const unsigned long long&);
	static long int fromHEX(string hexVal);
	static bool startsWith(const string& str, const string& prefix);
	static bool endsWith(const string& str, const string& suffix);
};

#endif /* STRINGUTIL_H_ */
