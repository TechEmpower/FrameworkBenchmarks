/*
 * TeBkRestController.cpp
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#include "TeBkRestController.h"

TeBkRestController::~TeBkRestController() {
	// TODO Auto-generated destructor stub
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
	sqli->startSession();
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
	sqli->startSession();
	for (int c = 0; c < queryCount; ++c) {
		int rid = rand() % 10000 + 1;
		GenericObject id;
		id << rid;
		TeBkWorld w = sqli->get<TeBkWorld>(id);
		w.setRandomNumber(rand() % 10000 + 1);
		wlst.push_back(w);
	}
	sqli->bulkUpdate<TeBkWorld>(wlst);
	sqli->endSession();
	delete sqli;
	return wlst;
}

std::string TeBkRestController::plaintext() {
	return "Hello, World!";
}
