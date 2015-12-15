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
 * TestObject1.h
 *
 *  Created on: 13-May-2013
 *      Author: sumeetc
 */

#ifndef TESTOBJECT1_H_
#define TESTOBJECT1_H_
#include "string"
#include "CastUtil.h"
using namespace std;

class TestObject {
	int a;
	long b;
	string c;
	float d;
	double e;
	bool f;
	long long g;
	short h;
	unsigned short i;
	unsigned int j;
	unsigned long k;
	unsigned long long l;
public:
	TestObject();
	virtual ~TestObject();
	int getA() const;
	void setA(int a);
	long getB() const;
	void setB(long b);
	string getC() const;
	void setC(string c);
	float getD() const;
	void setD(float d);
	double getE() const;
	void setE(double e);
	bool isF() const;
	void setF(bool f);
	long long getG() const;
	void setG(long long g);
	short getH() const;
	void setH(short h);
	unsigned short getI() const;
	void setI(unsigned short i);
	unsigned int getJ() const;
	void setJ(unsigned int j);
	unsigned long getK() const;
	void setK(unsigned long k);
	unsigned long long getL() const;
	void setL(unsigned long long l);
	string toString();
};

#endif /* TESTOBJECT1_H_ */
