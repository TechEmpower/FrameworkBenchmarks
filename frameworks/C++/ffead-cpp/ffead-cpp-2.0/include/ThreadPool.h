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
 * ThreadPool.h
 *
 *  Created on: Mar 23, 2010
 *      Author: sumeet
 */

#ifndef THREADPOOL_H_
#define THREADPOOL_H_
#include "TaskPool.h"
#include "PoolThread.h"
#include "LoggerFactory.h"
#include "FutureTask.h"

class ThreadPool {
	Logger logger;
	int maxThreads;
	int lowp;
	int highp;
	vector<PoolThread*> *tpool;
	TaskPool *wpool;
	bool prioritypooling;
	bool runFlag, pollerStarted;
	void initializeThreads();
	ConditionMutex *m_mutex;
	bool joinComplete;
	void initPointers();
public:
	ThreadPool(const int&, const int&, const int&);
	ThreadPool(const int&);
	ThreadPool();
	void start();
	void init(const int&);
	void joinAll();
	void submit(Task &task, const int& priority);
	void submit(Task &task);
	void schedule(Task &task, const long long& tunit, const int& type);
	void submit(Task *task, const int& priority);
	void submit(Task *task);
	void schedule(Task *task, const long long& tunit, const int& type);

	void submit(FutureTask &task, const int& priority);
	void submit(FutureTask &task);
	void schedule(FutureTask &task, const long long& tunit, const int& type);
	void submit(FutureTask *task, const int& priority);
	void submit(FutureTask *task);
	void schedule(FutureTask *task, const long long& tunit, const int& type);
	virtual ~ThreadPool();
};
#endif /* THREADPOOL_H_ */
