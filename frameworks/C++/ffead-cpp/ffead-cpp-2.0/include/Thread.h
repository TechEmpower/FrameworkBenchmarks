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
 * Thread.h
 *
 *  Created on: 10-Aug-2012
 *      Author: sumeetc
 */

#ifndef THREAD_H_
#define THREAD_H_
#include "AppDefines.h"
#include "Compatibility.h"

typedef void* (*ThreadFunc)(void*);

class Thread;

class ThreadFunctor
{
	friend class Thread;
	ThreadFunc f;
	void* arg;
};

class Thread {
	ThreadFunctor* threadFunctor;
	pthread_t pthread;
	pthread_cond_t cond;
	pthread_mutex_t mut;
	bool isDetached;
	static void* _service(void* arg);
public:
	Thread(const ThreadFunc& f, void* arg, const bool& isDetached= true);
	virtual ~Thread();
	void execute();
	void join();
	static void nSleep(const long& nanos);
	static void uSleep(const long& nanos);
	static void mSleep(const long& nanos);
	static void sSleep(const long& nanos);
	void wait();
	void interrupt();
};

#endif /* THREAD_H_ */
