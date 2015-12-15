/*
	Copyright 2009-2012, Sumeet Chhetri

    Licensed under the Apache License, Version 2.0 (the "License");
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
 * TestPage.h
 *
 *  Created on: Jul 13, 2011
 *      Author: sumeet
 */

#ifndef TESTPAGE_H_
#define TESTPAGE_H_
#include "string"
using namespace std;
class TestPage {
public:
	TestPage();
	virtual ~TestPage();
	string textonclick(int,string,string);
	int linkonclick();
};

#endif /* TESTPAGE_H_ */
