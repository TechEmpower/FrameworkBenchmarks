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
 * Mutex.h
 *
 *  Created on: 10-Aug-2012
 *      Author: sumeetc
 */

#ifndef MUTEX_H_
#define MUTEX_H_
#include <pthread.h>

class Mutex {
protected:
	pthread_mutex_t mut;
public:
	Mutex();
	virtual ~Mutex();
	void lock();
	void unlock();
};

class DummyMutex : public Mutex {
public:
	DummyMutex();
	virtual ~DummyMutex();
	void lock();
	void unlock();
};

class ReadWriteMutex {
	pthread_rwlock_t mut;
public:
	ReadWriteMutex();
	virtual ~ReadWriteMutex();
	void rlock();
	void wlock();
	void unlock();
};

class ConditionMutex : public Mutex {
	pthread_cond_t cond;
public:
	ConditionMutex();
	virtual ~ConditionMutex();
	void lock();
	void unlock();
	void conditionalWait();
	void conditionalNotifyOne();
	void conditionalNotifyAll();
	void wait();
	void interrupt();
};

#endif /* MUTEX_H_ */
