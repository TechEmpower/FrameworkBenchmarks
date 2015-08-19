/*
 * ConcurrentMap.h
 *
 *  Created on: 09-Mar-2015
 *      Author: sumeetc
 */

#ifndef CONCURRENTMAP_H_
#define CONCURRENTMAP_H_
#include "map"
#include "Mutex.h"

using namespace std;

template<typename K, typename V>
class ConcurrentMap {
	Mutex _l;
	map<K, V> _m;
public:
	void put(const K& k, const V& v)
	{
		_l.lock();
		_m[k] = v;
		_l.unlock();
	}
	void get(const K& k, V& v)
	{
		_l.lock();
		if(_m.find(k)!=_m.end())
		{
			v = _m[k];
		}
		_l.unlock();
	}
	bool find(const K& k)
	{
		bool fl = false;
		_l.lock();
		if(_m.find(k)!=_m.end())
		{
			fl = true;
		}
		_l.unlock();
		return fl;
	}
	bool find(const K& k, V& v)
	{
		bool fl = false;
		_l.lock();
		if(_m.find(k)!=_m.end())
		{
			v = _m[k];
			fl = true;
		}
		_l.unlock();
		return fl;
	}
	void erase(const K& k)
	{
		_l.lock();
		_m.erase(k);
		_l.unlock();
	}
};

#endif /* CONCURRENTMAP_H_ */
