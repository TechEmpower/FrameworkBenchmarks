/*
 * TeBkRestController.cpp
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#include "TeBkRestController.h"

TeBkRestController::TeBkRestController() {
}

TeBkRestController::~TeBkRestController() {
}

TeBkMessage TeBkRestController::json() {
	TeBkMessage msg;
	msg.setMessage("Hello, World!");
	return msg;
}

TeBkWorld TeBkRestController::db() {
	DataSourceInterface* sqli = DataSourceManager::getImpl();
	int rid = rand() % 10000 + 1;
	GenericObject id;
	id << rid;
	TeBkWorld w = sqli->get<TeBkWorld>(id);
	delete sqli;
	return w;
}

std::vector<TeBkWorld> TeBkRestController::queries(std::string queries) {
	std::vector<TeBkWorld> wlst;
	int queryCount = 1;
	try {
		queryCount = CastUtil::lexical_cast<int>(queries);
	} catch(...) {
	}
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	DataSourceInterface* sqli = DataSourceManager::getImpl();

	std::string tbName = "world";
	sqli->startSession(&tbName);
	for (int c = 0; c < queryCount; ++c) {
		int rid = rand() % 10000 + 1;
		GenericObject id;
		id << rid;
		TeBkWorld w = sqli->get<TeBkWorld>(id);
		wlst.push_back(w);
	}
	sqli->endSession();
	delete sqli;
	return wlst;
}

std::vector<TeBkWorld> TeBkRestController::updates(std::string queries) {
	std::vector<TeBkWorld> wlst;
	int queryCount = 1;
	try {
		queryCount = CastUtil::lexical_cast<int>(queries);
	} catch(...) {
	}
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	DataSourceInterface* sqli = DataSourceManager::getImpl();

	std::string tbName = "world";
	sqli->startSession(&tbName);
	for (int c = 0; c < queryCount; ++c) {
		int rid = rand() % 10000 + 1;
		GenericObject id;
		id << rid;
		TeBkWorld w = sqli->get<TeBkWorld>(id);
		int newRandomNumber = rand() % 10000 + 1;
		if(w.getRandomNumber() == newRandomNumber) {
			newRandomNumber -= 1;
		}
		w.setRandomNumber(newRandomNumber);
		wlst.push_back(w);
	}

	sqli->startTransaction();
	sqli->bulkUpdate<TeBkWorld>(wlst);
	sqli->commit();

	sqli->endSession();

	delete sqli;
	return wlst;
}

std::string TeBkRestController::plaintext() {
	return "Hello, World!";
}

void TeBkRestController::updateCache() {
	CacheInterface* cchi = CacheManager::getImpl();
	DataSourceInterface* sqli = DataSourceManager::getImpl();
	std::string tbName = "world";
	sqli->startSession(&tbName);
	for (int c = 1; c <= 10000; ++c) {
		GenericObject id;
		id << c;
		TeBkWorld w = sqli->get<TeBkWorld>(id);
		cchi->setO(CastUtil::lexical_cast<std::string>(c), w);
	}
	sqli->endSession();
	delete sqli;
	delete cchi;
}

std::vector<TeBkWorld> TeBkRestController::cachedWorlds(std::string count) {
	int queryCount = 1;
	try {
		queryCount = CastUtil::lexical_cast<int>(count);
	} catch(...) {
	}
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	CacheInterface* cchi = CacheManager::getImpl();
	std::vector<std::string> keys;
	for (int c = 0; c < queryCount; ++c) {
		int rid = rand() % 10000 + 1;
		keys.push_back(CastUtil::lexical_cast<std::string>(rid));
	}

	std::vector<TeBkWorld> wlst = cchi->mgetO<TeBkWorld>(keys);

	delete cchi;
	return wlst;
}
