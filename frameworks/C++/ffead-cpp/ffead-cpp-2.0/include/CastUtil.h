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
 * CastUtil.h
 *
 *  Created on: Aug 5, 2012
 *      Author: Sumeet
 */

#ifndef CASTUTIL_H_
#define CASTUTIL_H_
#include "sstream"
#include <stdlib.h>
#include "StringUtil.h"
#include <cxxabi.h>
#include "cstring"
#include <stdio.h>
#include <assert.h>
#include "map"
using namespace std;

class CastUtil {
	static map<string, string> _mangledClassNameMap;
	template <typename T> static string getClassName(T& t)
	{
		const char *mangled = typeid(t).name();
		string sm(mangled);
		if(_mangledClassNameMap.find(sm)!=_mangledClassNameMap.end()) {
			string tn = _mangledClassNameMap[sm];
			if(tn[tn.length()-1]=='*')
				tn = tn.substr(0,tn.length()-1);
			return tn;
		}
		int status;
		char *demangled;
		using namespace abi;
		demangled = __cxa_demangle(mangled, NULL, 0, &status);
		string tn(demangled);
		free(demangled);
		_mangledClassNameMap[sm] = tn;
		if(tn[tn.length()-1]=='*')
			tn = tn.substr(0,tn.length()-1);
		return tn;
	}
	template <typename T> static string* primitive(const T& val, const char* fmt)
	{
		const int n = snprintf(NULL, 0, fmt, val);
		char ty[n+1];
		memset (ty,0,n+1);
		int c = snprintf(ty, n+1, fmt, val);
		assert(strlen(ty)==n);
		assert(c == n);
		string* d = new string(ty, n);
		return d;
	}
public:
	static const string STD_STRING;
	CastUtil();
	virtual ~CastUtil();
	template <typename T, typename R> static R cast(const T& val)
	{
		return lexical_cast<R>(val);
	}

	template <typename T> static T lexical_cast(const short& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%d");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - short to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const unsigned short& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%d");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - unsigned short to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const int& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%d");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - int to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const unsigned int& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%u");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - unsigned int to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const long& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%ld");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - long to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const unsigned long& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%lu");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - unsigned long to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const long long& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%lld");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - long long to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const unsigned long long& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%llu");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - unsigned long long to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const double& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%f");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - double to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const long double& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%Lf");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - double to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const float& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = primitive(val, "%f");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - float to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const bool& val)
	{
		T t;
		string tn = getClassName(t);
		if(tn==STD_STRING)
		{
			void* d = NULL;
			if(val)
				d = new string("true");
			else
				d = new string("false");
			t = *(T*)d;
			delete ((string*)d);
			return t;
		}
		stringstream ss;
		ss << val;
		ss >> t;
		if(ss)
		{
			return t;
		}
		else
		{
			throw ("Conversion exception - bool to " + tn);
		}
	}
	template <typename T> static T lexical_cast(const string& vval)
	{
		T t;
		string tn = getClassName(t);
		char* endptr;
		if(tn=="double" || tn=="float")
		{
			double d = 0;
			if(vval.at(0)=='0' || vval.find("+0")==0 || vval.find("-0")==0)
			{
				int dots = StringUtil::countOccurrences(vval, ".");
				if(dots>1) {
					throw "Conversion exception - string to double";
				}
				if(vval.at(0)=='0')
				{
					if(vval.find_first_not_of(".0")==string::npos) {
						t = d;
					} else {
						d = strtod(vval.c_str(), &endptr);
						bool invalid = (d==0);
						if(invalid)
						{
							throw "Conversion exception - string to double";
						}
					}
				}
				else
				{
					if(vval.find_first_not_of(".0", 1)==string::npos) {
						t = d;
					} else {
						d = strtod(vval.substr(1).c_str(), &endptr);
						bool invalid = (d==0);
						if(invalid)
						{
							throw "Conversion exception - string to double";
						}
					}
				}
			}
			else
			{
				d = strtod(vval.c_str(), &endptr);
				bool invalid = (d==0);
				if(invalid)
				{
					throw "Conversion exception - string to double";
				}
			}
			t = d;
		}
		else if(tn=="long double")
		{
			long double d = 0;
			if(sscanf(vval.c_str(), "%Lf", &d)==1)
			{
				t = d;
			}
			else
			{
				throw "Conversion exception - string to long double";
			}
		}
		else if(tn=="int")
		{
			int d = 0;
			if(vval=="0" || (vval.at(0)=='0' && vval.find_first_not_of("0")==string::npos)
					|| (vval.find("+0")==0 && vval.find_first_not_of("0", 1)==string::npos)
					|| (vval.find("-0")==0 && vval.find_first_not_of("0", 1)==string::npos)) {
			} else {
				bool invalid = false;
				if(vval!="0")
				{
					d = strtol(vval.c_str(), &endptr, 10);
					invalid = (d==0);
				}
				if(invalid)
				{
					throw "Conversion exception - string to int";
				}
			}
			t = d;
		}
		else if(tn=="short")
		{
			short d = 0;
			if(vval=="0" || (vval.at(0)=='0' && vval.find_first_not_of("0")==string::npos)
					|| (vval.find("+0")==0 && vval.find_first_not_of("0", 1)==string::npos)
					|| (vval.find("-0")==0 && vval.find_first_not_of("0", 1)==string::npos)) {
			} else {
				bool invalid = false;
				if(vval!="0")
				{
					d = strtol(vval.c_str(), &endptr, 10);
					invalid = (d==0);
				}
				if(invalid)
				{
					throw "Conversion exception - string to short";
				}
			}
			t = d;
		}
		else if(tn=="long")
		{
			long d = 0;
			if(vval=="0" || (vval.at(0)=='0' && vval.find_first_not_of("0")==string::npos)
					|| (vval.find("+0")==0 && vval.find_first_not_of("0", 1)==string::npos)
					|| (vval.find("-0")==0 && vval.find_first_not_of("0", 1)==string::npos)) {
			} else {
				bool invalid = false;
				if(vval!="0")
				{
					d = strtol(vval.c_str(), &endptr, 10);
					invalid = (d==0);
				}
				if(invalid)
				{
					throw "Conversion exception - string to long";
				}
			}
			t = d;
		}
		else if(tn=="unsigned short")
		{
			unsigned short d = 0;
			if(vval=="0" || (vval.at(0)=='0' && vval.find_first_not_of("0")==string::npos)
					|| (vval.find("+0")==0 && vval.find_first_not_of("0", 1)==string::npos)
					|| (vval.find("-0")==0 && vval.find_first_not_of("0", 1)==string::npos)) {
			} else {
				bool invalid = false;
				if(vval!="0")
				{
					d = strtol(vval.c_str(), &endptr, 10);
					invalid = (d==0);
				}
				if(invalid)
				{
					throw "Conversion exception - string to unsigned short";
				}
			}
			t = d;
		}
		else if(tn=="unsigned int")
		{
			unsigned int d = 0;
			if(vval=="0" || (vval.at(0)=='0' && vval.find_first_not_of("0")==string::npos)
					|| (vval.find("+0")==0 && vval.find_first_not_of("0", 1)==string::npos)
					|| (vval.find("-0")==0 && vval.find_first_not_of("0", 1)==string::npos)) {
			} else {
				bool invalid = false;
				if(vval!="0")
				{
					d = strtol(vval.c_str(), &endptr, 10);
					invalid = (d==0);
				}
				if(invalid)
				{
					throw "Conversion exception - string to unsigned int";
				}
			}
			t = d;
		}
		else if(tn=="unsigned long")
		{
			unsigned long d = 0;
			if(vval=="0" || (vval.at(0)=='0' && vval.find_first_not_of("0")==string::npos)
					|| (vval.find("+0")==0 && vval.find_first_not_of("0", 1)==string::npos)
					|| (vval.find("-0")==0 && vval.find_first_not_of("0", 1)==string::npos)) {
			} else {
				bool invalid = false;
				if(vval!="0")
				{
					d = strtol(vval.c_str(), &endptr, 10);
					invalid = (d==0);
				}
				if(invalid)
				{
					throw "Conversion exception - string to unsigned long";
				}
			}
			t = d;
		}
		else if(tn=="long long")
		{
			long long d = -1;
			if(sscanf(vval.c_str(), "%lld", &d)==1)
			{
				t = d;
			}
			else
			{
				throw "Conversion exception - string to long long";
			}
		}
		else if(tn=="unsigned long long")
		{
			unsigned long long d = -1;
			if(sscanf(vval.c_str(), "%llu", &d)==1)
			{
				t = d;
			}
			else
			{
				throw "Conversion exception - string to unsigned long long";
			}
		}
		else if(tn=="bool")
		{
			bool d = false;
			if(StringUtil::toLowerCopy(vval)=="true" || StringUtil::toLowerCopy(vval)=="1")
				d = true;
			else if(StringUtil::toLowerCopy(vval)=="false" || StringUtil::toLowerCopy(vval)=="0")
				d = false;
			else
			{
				throw "Conversion exception - string to bool";
			}
			t = d;
		}
		else if(tn==STD_STRING || tn=="string")
		{
			string str = vval;
			if(vval.length()==0)return t;
			void* d = new string(vval);
			t = *(T*)d;
			delete ((string*)d);
		}
		else
		{
			throw "Generic Conversion exception";
		}
		return t;
	}
	template <typename T> static T lexical_cast(const char* val)
	{
		return lexical_cast<T>(val, strlen(val));
	}
	template <typename T> static T lexical_cast(const char* val, const size_t& len)
	{
		T t;
		string tn = getClassName(t);
		string vval(val, len);
		return lexical_cast<T>(vval);
	}
	template <typename T> static bool isPrimitiveDataType()
	{
		T t;
		string type = getClassName(t);
		if(type[type.length()-1]=='*')
			type = type.substr(0,type.length()-1);

		StringUtil::trim(type);
		if(type=="short" || type=="short int" || type=="signed short" || type=="signed short int"
				|| type=="unsigned short" || type=="unsigned short int"
				|| type=="signed" || type=="int" || type=="signed int"
				|| type=="unsigned" || type=="unsigned int" || type=="long"
				|| type=="long int" || type=="signed long" || type=="signed long int"
				|| type=="unsigned long" || type=="unsigned long int"
				|| type=="long long" || type=="long long int" || type=="signed long long"
				|| type=="signed long long int" || type=="unsigned long long"
				|| type=="unsigned long long int" || type=="long double" || type=="bool"
				|| type=="float" || type=="double" || type=="string" || type==STD_STRING
				|| type=="char" || type=="signed char" || type=="unsigned char"
				|| type=="wchar_t" ||  type=="char16_t" ||type=="char32_t")
		{
			return true;
		}
		return false;
	}
	template <typename T> static string getTypeName()
	{
		T t;
		string type = getClassName(t);
		if(type[type.length()-1]=='*')
			type = type.substr(0,type.length()-1);

		StringUtil::trim(type);
		if(type=="short" || type=="short int" || type=="signed short" || type=="signed short int")
		{
			return "short";
		}
		else if(type=="unsigned short" || type=="unsigned short int")
		{
			return "unsigned short";
		}
		else if(type=="signed" || type=="int" || type=="signed int")
		{
			return "int";
		}
		else if(type=="unsigned" || type=="unsigned int")
		{
			return "unsigned int";
		}
		else if(type=="long" || type=="long int" || type=="signed long" || type=="signed long int")
		{
			return "long";
		}
		else if(type=="unsigned long" || type=="unsigned long int")
		{
			return "unsigned long";
		}
		else if(type=="long long" || type=="long long int" || type=="signed long long" || type=="signed long long int")
		{
			return "long long";
		}
		else if(type=="unsigned long long" || type=="unsigned long long int")
		{
			return "unsigned long long";
		}
		else if(type=="long double")
		{
			return "long double";
		}
		return type;
	}
};

#endif /* CASTUTIL_H_ */
