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
 * FutureTask.h
 *
 *  Created on: 17-Mar-2013
 *      Author: sumeetc
 */

#ifndef FUTURETASK_H_
#define FUTURETASK_H_
#include "Task.h"
#include "Mutex.h"
#include "Thread.h"

class FutureTask : Task {
	friend class PoolThread;
	friend class ThreadPool;
	void* result;
	bool isComplete;
	Mutex m_mutex;
	void taskComplete();
	bool isTaskComplete();
public:
	FutureTask();
	FutureTask(const int& priority);
	FutureTask(const int& tunit, const int& type);
	virtual ~FutureTask();
	virtual void* call()=0;
	virtual void run(){}
	void* getResult();
};

#endif /* FUTURETASK_H_ */
