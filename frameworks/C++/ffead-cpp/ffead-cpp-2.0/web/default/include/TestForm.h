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
 * TestForm.h
 *
 *  Created on: Jul 14, 2011
 *      Author: sumeet
 */

#ifndef TESTFORM_H_
#define TESTFORM_H_
#include "string"
using namespace std;

class TestForm {
	int num;
	string txt;
	string che;
public:
	TestForm();
	virtual ~TestForm();
	string getChe() const;
    int getNum() const;
    string getTxt() const;
    void setChe(string che);
    void setNum(int num);
    void setTxt(string txt);
};

#endif /* TESTFORM_H_ */
