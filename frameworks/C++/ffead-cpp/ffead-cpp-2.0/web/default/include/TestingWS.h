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
 * TestingWS.h
 *
 *  Created on: 13-May-2013
 *      Author: sumeetc
 */

#ifndef TESTINGWS_H_
#define TESTINGWS_H_
#include "string"
#include "vector"
#include "TestObject.h"
#include "TestObject1.h"
#include "iostream"
#include "CastUtil.h"
using namespace std;

namespace ws {
	namespace test {
		class TestingWS {
		public:
			TestingWS();
			virtual ~TestingWS();
			void wsmeth1(int a, string, long);
			string wsmeth2(string b, vector<int> c);
			TestObject wsmeth3(string);
			com::obj::TestObject wsmeth4(bool);
			string wsmeth5(TestObject);
			long wsmeth6(com::obj::TestObject);
		};
	}
} /* namespace ws */
#endif /* TESTINGWS_H_ */
