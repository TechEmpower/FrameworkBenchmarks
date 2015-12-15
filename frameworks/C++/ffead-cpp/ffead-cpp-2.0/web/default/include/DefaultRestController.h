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
 * DefaultIOFilter.h
 *
 *  Created on: Apr 7, 2011
 *      Author: sumeet
 */

#ifndef DEFAULTRESTCONTROLLER_H_
#define DEFAULTRESTCONTROLLER_H_

#include <math.h>
#include <iostream>
#include "vector"
#include "TestMany.h"
#include <fstream>
#include "CastUtil.h"

class DefaultRestController {
public:
	DefaultRestController();
	virtual ~DefaultRestController();
	int addNumbers(int,int);
	double power(int,int);
	vector<int> testVector(vector<int>);
	TestMany testObject(TestMany);
	vector<TestMany> testVectorObject(vector<TestMany> param);
	string testUploadFile(ifstream* ifs, string param);
	string testUploadFileMulti1(ifstream* ifs1, ifstream* ifs2, ifstream* ifs3, string param);
	string testUploadFileMulti2(vector<ifstream*> vifs, string param);
};

#endif /* DEFAULTRESTCONTROLLER_H_ */
