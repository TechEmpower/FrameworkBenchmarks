/*
	Copyright 2010, Sumeet Chhetri

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
 * TestCronBasedJob.h
 *
 *  Created on: 15-Jul-2013
 *      Author: sumeetc
 */

#ifndef TESTCRONBASEDJOB_H_
#define TESTCRONBASEDJOB_H_
#include "string"
#include <iostream>
#include "CastUtil.h"
using namespace std;

class TestCronBasedJob {
	int counter;
public:
	TestCronBasedJob();
	virtual ~TestCronBasedJob();
	void runJob();
};

#endif /* TESTCRONBASEDJOB_H_ */
