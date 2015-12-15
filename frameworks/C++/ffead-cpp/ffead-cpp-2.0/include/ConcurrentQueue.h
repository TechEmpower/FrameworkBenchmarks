/*
 * ConcurrentQueue.h
 *
 *  Created on: 08-Mar-2015
 *      Author: sumeetc
 */

#ifndef CONCURRENTQUEUE_H_
#define CONCURRENTQUEUE_H_
#include "queue"
#include "Mutex.h"

template<typename T>
class ConcurrentQueue {
	Mutex _m;
	std::queue<T> _q;
public:
	void push(const T& t)
	{
		_m.lock();
		_q.push(t);
		_m.unlock();
	}
	bool pop(T& t)
	{
		bool fl = false;
		_m.lock();
		if(!_q.empty()) {
			t = _q.front();
			_q.pop();
			fl = true;
		}
		_m.unlock();
		return fl;
	}
};

#endif /* CONCURRENTQUEUE_H_ */
