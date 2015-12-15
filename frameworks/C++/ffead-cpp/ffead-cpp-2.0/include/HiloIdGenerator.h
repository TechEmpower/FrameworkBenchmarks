/*
 * HiloIdGenerator.h
 *
 *  Created on: 24-Sep-2014
 *      Author: sumeetc
 */

#ifndef HILOIDGENERATOR_H_
#define HILOIDGENERATOR_H_
#include "string"
#include "map"
#include "Mutex.h"
using namespace std;

class HiloIdGenerator {
	static map<string, long long> idsInSession;
	static map<string, long long> hiloIdMaxValuesInSession;
	static map<string, Mutex> locks;
	static string ALL;
public:
	static void init(const string& name, const long long& id, const int& lowValue, const bool& forceReinit= false);
	static void init(const long long& id, const int& lowValue, const bool& forceReinit= false);
	static bool isInitialized(const string& name);
	static bool isInitialized();

	static long long next();
	static long long next(const string& name);
};

#endif /* HILOIDGENERATOR_H_ */
