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
 * Bigdecimal.h
 *
 *  Created on: 06-Mar-2013
 *      Author: sumeetc
 */

#ifndef BIGDECIMAL_H_
#define BIGDECIMAL_H_
#include "Bigint.h"

class Bigdecimal {
	vector<int> parts;
	bool isPositive;
	int decimalDigits;
	int decimalStartsAt;
	void create(const string& value);
	void checkAndSetIfZero();
public:
	Bigdecimal();
	Bigdecimal(const string& value);
	void add(const Bigdecimal& number);
	Bigdecimal operator+(const Bigdecimal& number);
	Bigdecimal operator-(const Bigdecimal& number);
	Bigdecimal operator*(const Bigdecimal& number);
	Bigdecimal operator/(const Bigdecimal& number);
	Bigdecimal& operator++();
	Bigdecimal& operator+=(const Bigdecimal& number);
	Bigdecimal& operator--();
	Bigdecimal& operator-=(const Bigdecimal& number);
	friend bool operator== (const Bigdecimal& lhs, const Bigdecimal& rhs);
	friend bool operator!= (const Bigdecimal& lhs, const Bigdecimal& rhs);
	friend bool operator< (const Bigdecimal& lhs, const Bigdecimal& rhs);
	friend bool operator<= (const Bigdecimal& lhs, const Bigdecimal& rhs);
	friend bool operator> (const Bigdecimal& lhs, const Bigdecimal& rhs);
	friend bool operator>= (const Bigdecimal& lhs, const Bigdecimal& rhs);
	void subtract(const Bigdecimal& number);
	void multiply(const Bigdecimal& number);
	void divide(const Bigdecimal& number, const int& precision=15);
	static int compare(const Bigdecimal& number1, const Bigdecimal& number2);
	int compare(const Bigdecimal& number) const;
	string toString() const;
	virtual ~Bigdecimal();
};

#endif /* BIGDECIMAL_H_ */
