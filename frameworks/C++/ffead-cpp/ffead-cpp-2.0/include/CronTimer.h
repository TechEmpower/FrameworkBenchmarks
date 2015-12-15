/*
	Copyright 2010, Sumeet Chhetri

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
 * CronTimer.h
 *
 *  Created on: 06-Jul-2013
 *      Author: sumeetc
 */

#ifndef CRONTIMER_H_
#define CRONTIMER_H_
#include "AppDefines.h"
#include "Compatibility.h"
#include <algorithm>
#include <iostream>
#include "vector"
#include "string"
#include "map"
#include "Date.h"
#include "CastUtil.h"
using namespace std;

class CronTimer {
	class CronExpressionPart
	{
		bool all;
		int from;
		int to;
		int interval;
		string list;
		int nextRun;
		vector<int> lstParts;
		friend class CronTimer;
		CronExpressionPart()
		{
			interval = -1;
			from = -1;
			to = -1;
			all = false;
			nextRun = -1;
		}
	};
	vector<CronExpressionPart> cParts;
	Date* nextRunDate;
	vector<int> starts, ends, lstParts;
	map<string, int> months, days;
	friend class JobScheduler;
public:
	CronTimer(const string& cronExpression);
	virtual ~CronTimer();
	string toString();
	bool isValid(const int& pos, const int& cvalue, const int& nvalue);
	void correctNextRunDateTime(const int& pos);
	bool tryIncrement(const int& pos, const int& cvalue);
	void run();
};

#endif /* CRONTIMER_H_ */
