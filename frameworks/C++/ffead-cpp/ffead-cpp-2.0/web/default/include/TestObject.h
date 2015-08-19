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
 * TestObject.h
 *
 *  Created on: 13-May-2013
 *      Author: sumeetc
 */

#ifndef TESTOBJECT_H_
#define TESTOBJECT_H_
#include "string"
#include "vector"
#include "CastUtil.h"
using namespace std;

namespace com {
	namespace obj {
		class TestObject {
			vector<short> a;
			vector<int> b;
			vector<long> c;
			vector<long long> d;
			vector<unsigned short> e;
			vector<unsigned int> f;
			vector<unsigned long> g;
			vector<unsigned long long> h;
			vector<float> i;
			vector<double> j;
			vector<bool> k;
			vector<string> l;
		public:
			TestObject();
			virtual ~TestObject();
			vector<short> getA() const;
			void setA(vector<short> a);
			vector<int> getB() const;
			void setB(vector<int> b);
			vector<long> getC() const;
			void setC(vector<long> c);
			vector<long long> getD() const;
			void setD(vector<long long> d);
			vector<unsigned short> getE() const;
			void setE(vector<unsigned short> e);
			vector<unsigned int> getF() const;
			void setF(vector<unsigned int> f);
			vector<unsigned long> getG() const;
			void setG(vector<unsigned long> g);
			vector<unsigned long long> getH() const;
			void setH(vector<unsigned long long> h);
			vector<float> getI() const;
			void setI(vector<float> i);
			vector<double> getJ() const;
			void setJ(vector<double> j);
			vector<bool> getK() const;
			void setK(vector<bool> k);
			vector<string> getL() const;
			void setL(vector<string> l);
			string toString();
		};
	}
} /* namespace com */
#endif /* TESTOBJECT_H_ */
