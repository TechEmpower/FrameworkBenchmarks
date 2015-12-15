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
 * Bigint.h
 *
 *  Created on: 04-Mar-2013
 *      Author: sumeetc
 */

#ifndef BIGINT_H_
#define BIGINT_H_
#include "string"
#include "vector"
#include "algorithm"
#include "CastUtil.h"
#include <iostream>
using namespace std;


class Bigint {
	friend class Bigdecimal;
	vector<int> parts;
	bool isPositive;
	int decimalStartsAt;
	void create(const string& value);
	void checkAndSetIfZero();
	int decompose(string fnvalue, string snvalue, const Bigint& number, int recurse, string& build, const bool& isDecimal, const int&);
	void internalAdd(const Bigint& number);
	void internalDivide(const Bigint& number, const bool& isDecimal, const int&);
public:
	static string BLANK;
	static string MINUS;
	static string ZERO;
	static int ZERO_INT;
	static int NUM_LENGTH;
	static int NUM_MAX;
	static int NUM_MAX_THRESHOLD;
	Bigint();
	Bigint(const string& value);
	void add(const Bigint& number);
	Bigint operator+(const Bigint& number);
	Bigint operator-(const Bigint& number);
	Bigint operator*(const Bigint& number);
	Bigint operator/(const Bigint& number);
	Bigint& operator++();
	Bigint& operator+=(const Bigint& number);
	Bigint& operator--();
	Bigint& operator-=(const Bigint& number);
	friend bool operator== (const Bigint &lhs, const Bigint &rhs);
	friend bool operator!= (const Bigint &lhs, const Bigint &rhs);
	friend bool operator< (const Bigint &lhs, const Bigint &rhs);
	friend bool operator<= (const Bigint &lhs, const Bigint &rhs);
	friend bool operator> (const Bigint &lhs, const Bigint &rhs);
	friend bool operator>= (const Bigint &lhs, const Bigint &rhs);
	void subtract(const Bigint& number);
	void multiply(const Bigint& number);
	void divide(const Bigint& number);
	static int compare(const Bigint& number1, const Bigint& number2);
	int compare(const Bigint& number) const;
	string toString() const;
	virtual ~Bigint();
};

#endif /* BIGINT_H_ */
