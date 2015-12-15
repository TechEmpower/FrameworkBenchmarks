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

#ifndef AMEFRESOURCES_H_
#define AMEFRESOURCES_H_

#include "AMEFEncoder.h"
#include "AMEFDecoder.h"

class AMEFResources
{
	AMEFResources(){}
	~AMEFResources();
public:

	static char* longTocharArray(const long& l, const int& ind);
	static string longTocharArrayS(const long& l, const int& ind);
	static char* intTocharArray(const int& l, const int& ind);
	static char* intTocharArrayWI(const int& l);
	static int charArrayToInt(char* l, const int& off, const int& ind);
	static long charArrayToLong(char* l, const int& off, const int& ind);
	static long charArrayToLong(char* l, const int& ind);
	static string intTocharArrayS(const int& l, const int& ind);
	#ifdef IS_64_BIT
		static string longTocharArrayWI(const unsigned long long& l);
		static string longTocharArrayWI(const long& l);
	#else
		static string longTocharArrayWI(const unsigned long long& l);
		static string longTocharArrayWI(const long& l);
	#endif
};

#endif /* AMEFRESOURCES_H_ */
