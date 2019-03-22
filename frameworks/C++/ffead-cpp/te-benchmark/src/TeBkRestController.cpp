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
	try {
		TeBkWorld w = sqli->get<TeBkWorld>(id);
		delete sqli;
		return w;
	} catch(const std::exception& e) {
		delete sqli;
		throw e;
	}
}

std::vector<TeBkWorld> TeBkRestController::queries(std::string queries) {
	std::vector<TeBkWorld> wlst;
	int queryCount = 1;
	try {
		queryCount = CastUtil::lexical_cast<int>(queries);
	} catch(const std::exception& e) {
	}
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	DataSourceInterface* sqli = DataSourceManager::getImpl();

	try {
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
	} catch(const std::exception& e) {
		delete sqli;
		throw e;
	}
}

std::vector<TeBkWorld> TeBkRestController::updates(std::string queries) {
	std::vector<TeBkWorld> wlst;
	int queryCount = 1;
	try {
		queryCount = CastUtil::lexical_cast<int>(queries);
	} catch(const std::exception& e) {
	}
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	DataSourceInterface* sqli = DataSourceManager::getImpl();

	try {
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
	} catch(const std::exception& e) {
		delete sqli;
		throw e;
	}
}

std::string TeBkRestController::plaintext() {
	return "Hello, World!";
}

void TeBkRestController::updateCache() {
	CacheInterface* cchi = CacheManager::getImpl();
	DataSourceInterface* sqli = DataSourceManager::getImpl();

	FileBasedLock fl("/tmp/cache.lock");
	try {
		fl.lock();
		try {
			std::string tbName = "world";
			sqli->startSession(&tbName);
			std::vector<TeBkWorld> wlist = sqli->getAll<TeBkWorld>();
			sqli->endSession();
			for (int c = 0; c < (int)wlist.size(); ++c) {
				TeBkWorld& w = wlist.at(c);
				cchi->setO(CastUtil::lexical_cast<std::string>(w.getId()), w);
			}
			delete sqli;
			delete cchi;
		} catch(const std::exception& e) {
			delete sqli;
			delete cchi;
			throw e;
		}
		fl.unlock();
	} catch(const std::exception& e) {
		//In case of apache/nginx the other child processes need to wait for the cache to be primed
		//Thread::sSleep(10);
	}
}

std::vector<TeBkWorld> TeBkRestController::cachedWorlds(std::string count) {
	int queryCount = 1;
	try {
		queryCount = CastUtil::lexical_cast<int>(count);
	} catch(const std::exception& e) {
	}
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	CacheInterface* cchi = CacheManager::getImpl();

	try {
		std::vector<std::string> keys;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			keys.push_back(CastUtil::lexical_cast<std::string>(rid));
		}

		std::vector<TeBkWorld> wlst = cchi->mgetO<TeBkWorld>(keys);

		delete cchi;
		return wlst;
	} catch(const std::exception& e) {
		delete cchi;
		throw e;
	}
}

