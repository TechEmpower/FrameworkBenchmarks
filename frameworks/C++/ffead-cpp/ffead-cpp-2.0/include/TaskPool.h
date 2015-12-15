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
 * TaskPool.h
 *
 *  Created on: Mar 23, 2010
 *      Author: sumeet
 */

#ifndef TASKPOOL_H_
#define TASKPOOL_H_
#include "AppDefines.h"
#include "vector"
#include "queue"
#include "list"
#include "Task.h"
#include "Mutex.h"
#include "Thread.h"
#include "TimeUnit.h"
#include "Timer.h"
#ifdef HAVE_CXX11
#include "atomic"
#endif

class TaskPool {
	std::queue<Task*> *tasks;
	priority_queue<Task*> *ptasks;
	vector<Task*> *scheduledtasks;
	vector<Timer*> *scheduledTimers;
	Mutex *s_mutex;
	ConditionMutex *c_mutex;
	Thread *mthread;
	int count;
	static void* run(void *arg);
	friend class ThreadPool;
	friend class PoolThread;
	bool prioritybased;
	std::atomic<bool> runFlag, complete, thrdStarted;
public:
	TaskPool(bool prioritybased);
	~TaskPool();
	void addTask(Task &task);
	void addTask(Task *task);
	void addSTask(Task &task);
	void addSTask(Task *task);
	void addPTask(Task &task);
	void addPTask(Task *task);
	Task* getTask();
	Task* getPTask();
	bool tasksPending();
	bool tasksPPending();
	void start();
};

#endif /* TASKPOOL_H_ */
