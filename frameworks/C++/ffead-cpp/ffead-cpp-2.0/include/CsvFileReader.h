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
 * CsvFileReader.h
 *
 *  Created on: 29-Jan-2013
 *      Author: sumeetc
 */

#ifndef CSVFILEREADER_H_
#define CSVFILEREADER_H_
#include "iostream"
#include "fstream"
#include "sstream"
#include "map"
#include "vector"
#include "StringUtil.h"

typedef vector<vector<string> > strVecVec;

class CsvFileReader {
public:
	CsvFileReader();
	virtual ~CsvFileReader();
	strVecVec getRows(const string&);
};

#endif /* CSVFILEREADER_H_ */
