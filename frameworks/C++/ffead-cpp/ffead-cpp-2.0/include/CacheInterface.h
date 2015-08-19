/*
 * CacheInterface.h
 *
 *  Created on: 07-May-2014
 *      Author: sumeetc
 */

#ifndef CACHEINTERFACE_H_
#define CACHEINTERFACE_H_
#include "CastUtil.h"
#include "Connection.h"
#include "GenericObject.h"
#include "map"

class CacheInterface {
public:
	CacheInterface();
	virtual ~CacheInterface();
	virtual void init()=0;

	bool set(const string& key, const char* value, const int& expireSeconds);
	bool add(const string& key, const char* value, const int& expireSeconds);
	bool replace(const string& key, const char* value, const int& expireSeconds);

	bool set(const string& key, const string& value, const int& expireSeconds);
	bool add(const string& key, const string& value, const int& expireSeconds);
	bool replace(const string& key, const string& value, const int& expireSeconds);

	bool set(const string& key, const short& value, const int& expireSeconds);
	bool add(const string& key, const short& value, const int& expireSeconds);
	bool replace(const string& key, const short& value, const int& expireSeconds);
	bool set(const string& key, const unsigned short& value, const int& expireSeconds);
	bool add(const string& key, const unsigned short& value, const int& expireSeconds);
	bool replace(const string& key, const unsigned short& value, const int& expireSeconds);

	bool set(const string& key, const int& value, const int& expireSeconds);
	bool add(const string& key, const int& value, const int& expireSeconds);
	bool replace(const string& key, const int& value, const int& expireSeconds);
	bool set(const string& key, const unsigned int& value, const int& expireSeconds);
	bool add(const string& key, const unsigned int& value, const int& expireSeconds);
	bool replace(const string& key, const unsigned int& value, const int& expireSeconds);

	bool set(const string& key, const long& value, const int& expireSeconds);
	bool add(const string& key, const long& value, const int& expireSeconds);
	bool replace(const string& key, const long& value, const int& expireSeconds);
	bool set(const string& key, const unsigned long& value, const int& expireSeconds);
	bool add(const string& key, const unsigned long& value, const int& expireSeconds);
	bool replace(const string& key, const unsigned long& value, const int& expireSeconds);

	bool set(const string& key, const long long& value, const int& expireSeconds);
	bool add(const string& key, const long long& value, const int& expireSeconds);
	bool replace(const string& key, const long long& value, const int& expireSeconds);
	bool set(const string& key, const unsigned long long& value, const int& expireSeconds);
	bool add(const string& key, const unsigned long long& value, const int& expireSeconds);
	bool replace(const string& key, const unsigned long long& value, const int& expireSeconds);

	bool set(const string& key, const float& value, const int& expireSeconds);
	bool add(const string& key, const float& value, const int& expireSeconds);
	bool replace(const string& key, const float& value, const int& expireSeconds);

	bool set(const string& key, const double& value, const int& expireSeconds);
	bool add(const string& key, const double& value, const int& expireSeconds);
	bool replace(const string& key, const double& value, const int& expireSeconds);
	bool set(const string& key, const long double& value, const int& expireSeconds);
	bool add(const string& key, const long double& value, const int& expireSeconds);
	bool replace(const string& key, const long double& value, const int& expireSeconds);

	bool set(const string& key, const bool& value, const int& expireSeconds);
	bool add(const string& key, const bool& value, const int& expireSeconds);
	bool replace(const string& key, const bool& value, const int& expireSeconds);

	virtual bool set(const string& key, GenericObject& value, const int& expireSeconds)=0;
	virtual bool add(const string& key, GenericObject& value, const int& expireSeconds)=0;
	virtual bool replace(const string& key, GenericObject& value, const int& expireSeconds)=0;
	virtual bool remove(const string& key)=0;
	virtual long long increment(const string& key, const int& number= 1)=0;
	virtual long long decrement(const string& key, const int& number= 1)=0;
	virtual long double incrementFloat(const string& key, const double& number = 1.0)=0;
	virtual long double decrementFloat(const string& key, const double& number = 1.0)=0;
	virtual map<string, string> statistics()=0;
	virtual bool flushAll()=0;
	virtual string getValue(const string& key)=0;

	template<typename T> inline T get(const string& key) {
		string val = getValue(key);
		return GenericObject::getObjectFromSerilaizedState<T>(val);
	}

	virtual void* executeCommand(const string& command, ...)=0;
};

#endif /* CACHEINTERFACE_H_ */
