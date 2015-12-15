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
 * PoolThread.h
 *
 *  Created on: Mar 23, 2010
 *      Author: sumeet
 */

#ifndef POOLTHREAD_H_
#define POOLTHREAD_H_
#include "TaskPool.h"
#include "Thread.h"
#include "Mutex.h"
#include "TimeUnit.h"
#include "LoggerFactory.h"
#include "FutureTask.h"
#ifdef HAVE_CXX11
#include "atomic"
#endif

class PoolThread {
	static void* run(void *arg);
	PoolThread(TaskPool* wpool);
	virtual ~PoolThread();
	Thread *mthread;
	bool idle;
	Task *task;
	Logger logger;
	Mutex *m_mutex;
	TaskPool* wpool;
	friend class ThreadPool;
	bool prioritybased;
	std::atomic<bool> runFlag, complete, thrdStarted;
public:
	void execute();
	void stop();
	bool isComplete();
};

#endif /* POOLTHREAD_H_ */
