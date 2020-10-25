/*
	Copyright 2009-2020, Sumeet Chhetri

    Licensed under the Apache License, Version 2.0 (the "License");
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
 * TeBkUmUm.cpp
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */
#include "TeBkUm.h"

int TeBkUmFortune::getId() const {
	return id;
}

void TeBkUmFortune::setId(int id) {
	this->id = id;
}

const std::string& TeBkUmFortune::getMessage() const {
	return message;
}

void TeBkUmFortune::setMessage(const std::string& message) {
	this->message = message;
}

TeBkUmFortune::TeBkUmFortune() {
	id = 0;
}

TeBkUmFortune::~TeBkUmFortune() {
}

bool TeBkUmFortune::operator < (const TeBkUmFortune& other) const {
	return message.compare(other.message)<0;
}

TeBkUmMessage::~TeBkUmMessage() {
}

const std::string& TeBkUmMessage::getMessage() const {
	return message;
}

void TeBkUmMessage::setMessage(const std::string& message) {
	this->message = message;
}

const std::string TeBkUmRouter::HELLO_WORLD = "Hello, World!";
std::string TeBkUmRouter::WORLD = "world";

void TeBkUmRouter::db(TeBkUmWorld& w) {
	DataSourceInterface* sqli = DataSourceManager::getImpl();
	int rid = rand() % 10000 + 1;
	GenericObject id;
	id << rid;
	try {
		w = sqli->get<TeBkUmWorld>(id);
		DataSourceManager::cleanImpl(sqli);
	} catch(const std::exception& e) {
		DataSourceManager::cleanImpl(sqli);
		throw e;
	}
}

void TeBkUmRouter::queries(const char* q, int ql, std::vector<TeBkUmWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	DataSourceInterface* sqli = DataSourceManager::getImpl();

	try {
		sqli->startSession(&WORLD);
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			GenericObject id;
			id << rid;
			TeBkUmWorld w = sqli->get<TeBkUmWorld>(id);
			wlst.push_back(w);
		}
		sqli->endSession();
		DataSourceManager::cleanImpl(sqli);
	} catch(const std::exception& e) {
		DataSourceManager::cleanImpl(sqli);
		throw e;
	}
}

void TeBkUmRouter::updates(const char* q, int ql, std::vector<TeBkUmWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	DataSourceInterface* sqli = DataSourceManager::getImpl();

	try {
		sqli->startSession(&WORLD);
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			GenericObject id;
			id << rid;
			TeBkUmWorld w = sqli->get<TeBkUmWorld>(id);
			int newRandomNumber = rand() % 10000 + 1;
			if(w.getRandomNumber() == newRandomNumber) {
				newRandomNumber += 1;
				if(newRandomNumber>=10000) {
					newRandomNumber = 1;
				}
			}
			w.setRandomNumber(newRandomNumber);
			wlst.push_back(w);
		}

		sqli->startTransaction();
		sqli->bulkUpdate<TeBkUmWorld>(wlst);
		sqli->commit();

		sqli->endSession();
		DataSourceManager::cleanImpl(sqli);
	} catch(const std::exception& e) {
		DataSourceManager::cleanImpl(sqli);
		throw e;
	}
}

void TeBkUmRouter::updateCache() {
	CacheInterface* cchi = CacheManager::getImpl();
	DataSourceInterface* sqli = DataSourceManager::getImpl();

	try {
		sqli->startSession(&WORLD);
		std::vector<TeBkUmWorld> wlist = sqli->getAll<TeBkUmWorld>();
		sqli->endSession();
		for (int c = 0; c < (int)wlist.size(); ++c) {
			TeBkUmWorld& w = wlist.at(c);
			cchi->setO(CastUtil::fromNumber(w.getId()), w);
		}
		DataSourceManager::cleanImpl(sqli);
		CacheManager::cleanImpl(cchi);
	} catch(const std::exception& e) {
		DataSourceManager::cleanImpl(sqli);
		CacheManager::cleanImpl(cchi);
		throw e;
	}
}

void TeBkUmRouter::cachedWorlds(const char* q, int ql, std::vector<TeBkUmWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	CacheInterface* cchi = CacheManager::getImpl();

	try {
		std::vector<std::string> keys;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			keys.push_back(CastUtil::fromNumber(rid));
		}

		wlst = cchi->mgetO<TeBkUmWorld>(keys);
		CacheManager::cleanImpl(cchi);
	} catch(const std::exception& e) {
		CacheManager::cleanImpl(cchi);
		throw e;
	}
}

void TeBkUmRouter::getContext(HttpRequest* request, Context* context) {
	DataSourceInterface* sqli = DataSourceManager::getImpl();

	try {
		std::vector<TeBkUmFortune> flstT = sqli->getAll<TeBkUmFortune>();
		std::vector<TeBkUmFortune>* flst = new std::vector<TeBkUmFortune>;
		flst->swap(flstT);

		for(int i=0;i<(int)flst->size();i++)
		{
			std::string nm = flst->at(i).getMessage();
			CryptoHandler::sanitizeHtml(nm);
			flst->at(i).setMessage(nm);
		}

		TeBkUmFortune nf;
		nf.setId(0);
		nf.setMessage("Additional fortune added at request time.");
		flst->push_back(nf);
		std::sort (flst->begin(), flst->end());

		context->insert(std::pair<std::string, void*>("fortunes", flst));

		DataSourceManager::cleanImpl(sqli);
	} catch(...) {
		DataSourceManager::cleanImpl(sqli);
		throw;
	}
}

//https://stackoverflow.com/questions/9631225/convert-strings-specified-by-length-not-nul-terminated-to-int-float
bool TeBkUmRouter::strToNum(const char* str, int len, int& ret) {
    ret = 0;
    for(int i = 0; i < len; ++i)
    {
    	if(!isdigit(str[i])) return false;
        ret = ret * 10 + (str[i] - '0');
    }
    return true;
}

bool TeBkUmRouter::route(HttpRequest* req, HttpResponse* res, void* dlib, void* ddlib, SocketInterface* sif) {
	//Timer t;
	//t.start();
	std::string_view path = req->getPath();
	if(StringUtil::endsWith(path, "/plaintext")) {
		//t.end();
		//CommonUtils::tsContRstLkp += t.timerNanoSeconds();
		//t.start();
		res->setContent(HELLO_WORLD);
		res->setContentType(ContentTypes::CONTENT_TYPE_TEXT_PLAIN);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		//t.end();
		//CommonUtils::tsContRstSer += t.timerNanoSeconds();
	} else if(StringUtil::endsWith(path, "/json")) {
		//t.end();
		//CommonUtils::tsContRstLkp += t.timerNanoSeconds();
		//t.start();
		TeBkUmMessage msg;
		msg.setMessage(HELLO_WORLD);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 0, "TeBkUmMessage"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		//t.end();
		//CommonUtils::tsContRstSer += t.timerNanoSeconds();
	} else if(StringUtil::endsWith(path, "/db")) {
		//t.end();
		//CommonUtils::tsContRstLkp += t.timerNanoSeconds();
		//t.start();
		TeBkUmWorld msg;
		db(msg);
		//t.end();
		//CommonUtils::tsContExec += t.timerNanoSeconds();
		//t.start();
		res->setContent(JSONSerialize::serializeUnknown(&msg, 0, "TeBkUmWorld"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		//t.end();
		//CommonUtils::tsContRstSer += t.timerNanoSeconds();
	} else if(StringUtil::endsWith(path, "/queries")) {
		//t.end();
		//CommonUtils::tsContRstLkp += t.timerNanoSeconds();
		//t.start();
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmWorld> msg;
		queries(params[0].val, params[0].val_len, msg);
		//t.end();
		//CommonUtils::tsContExec += t.timerNanoSeconds();
		//t.start();
		res->setContent(JSONSerialize::serializeUnknown(&msg, 100, "std::vector<TeBkUmWorld>"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		//t.end();
		//CommonUtils::tsContRstSer += t.timerNanoSeconds();
	} else if(StringUtil::endsWith(path, "/fortunes")) {
		Context ctx;
		getContext(req, &ctx);

		void* mkr = dlsym(ddlib, TPE_FN_NAME.c_str());
		if(mkr!=NULL)
		{
			TeBkUmTemplatePtr f =  (TeBkUmTemplatePtr)mkr;
			std::string msg;
			f(&ctx, msg);
			res->setContent(msg);
			res->setContentType(ContentTypes::CONTENT_TYPE_TEXT_SHTML);
			res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		}
	} else if(StringUtil::endsWith(path, "/updates")) {
		//t.end();
		//CommonUtils::tsContRstLkp += t.timerNanoSeconds();
		//t.start();
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmWorld> msg;
		updates(params[0].val, params[0].val_len, msg);
		//t.end();
		//CommonUtils::tsContExec += t.timerNanoSeconds();
		//t.start();
		res->setContent(JSONSerialize::serializeUnknown(&msg, 100, "std::vector<TeBkUmWorld>"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		//t.end();
		//CommonUtils::tsContRstSer += t.timerNanoSeconds();
	} else if(StringUtil::endsWith(path, "/cached-worlds")) {
		//t.end();
		//CommonUtils::tsContRstLkp += t.timerNanoSeconds();
		//t.start();
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmWorld> msg;
		cachedWorlds(params[0].val, params[0].val_len, msg);
		//t.end();
		//CommonUtils::tsContExec += t.timerNanoSeconds();
		//t.start();
		res->setContent(JSONSerialize::serializeUnknown(&msg, 100, "std::vector<TeBkUmWorld>"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		//t.end();
		//CommonUtils::tsContRstSer += t.timerNanoSeconds();
	} else {
		res->setHTTPResponseStatus(HTTPResponseStatus::NotFound);
	}
	res->setDone(true);
	return true;
}

std::string TeBkUmRouter::APP_NAME = "";
std::string TeBkUmRouter::TPE_FN_NAME = "";

TeBkUmRouter::TeBkUmRouter() {
	if(APP_NAME=="") {
		APP_NAME = CommonUtils::normalizeAppName("te-benchmark-um");
		TPE_FN_NAME = CommonUtils::getTpeFnName("tpe/fortunes.tpe", "te-benchmark-um");
	}
}

TeBkUmRouter::~TeBkUmRouter() {
}
