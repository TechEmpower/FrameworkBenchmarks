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
 * TeBkUmLpqUm.cpp
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */
#include "TeBkUmLpq.h"

int TeBkUmLpqWorld::getId() const {
	return id;
}

void TeBkUmLpqWorld::setId(int id) {
	this->id = id;
}

int TeBkUmLpqWorld::getRandomNumber() const {
	return randomNumber;
}

void TeBkUmLpqWorld::setRandomNumber(int randomNumber) {
	this->randomNumber = randomNumber;
}

TeBkUmLpqWorld::TeBkUmLpqWorld() {
	id = 0;
	randomNumber = 0;
}

TeBkUmLpqWorld::~TeBkUmLpqWorld() {
}

int TeBkUmLpqFortune::getId() const {
	return id;
}

void TeBkUmLpqFortune::setId(int id) {
	this->id = id;
}

const std::string& TeBkUmLpqFortune::getMessage() const {
	return message;
}

void TeBkUmLpqFortune::setMessage(const std::string& message) {
	this->message = message;
}

TeBkUmLpqFortune::TeBkUmLpqFortune() {
	id = 0;
}

TeBkUmLpqFortune::~TeBkUmLpqFortune() {
}

bool TeBkUmLpqFortune::operator < (const TeBkUmLpqFortune& other) const {
	return message.compare(other.message)<0;
}

TeBkUmLpqMessage::~TeBkUmLpqMessage() {
}

const std::string& TeBkUmLpqMessage::getMessage() const {
	return message;
}

void TeBkUmLpqMessage::setMessage(const std::string& message) {
	this->message = message;
}

const std::string TeBkUmLpqRouter::HELLO_WORLD = "Hello, World!";
std::string TeBkUmLpqRouter::WORLD = "world";
std::string TeBkUmLpqRouter::WORLD_ONE_QUERY = "select id, randomnumber from world where id = $1";
std::string TeBkUmLpqRouter::WORLD_ALL_QUERY = "select id, randomnumber from world";
std::string TeBkUmLpqRouter::FORTUNE_ALL_QUERY = "select id, message from fortune";

void TeBkUmLpqRouter::db(TeBkUmLpqWorld& w) {
	LibpqDataSourceImpl* sqli = getDb();
	int rid = rand() % 10000 + 1;
	try {
		std::vector<LibpqParam> pars;
		LibpqDataSourceImpl::ADD_INT4(pars, rid);
		sqli->executeQuery(WORLD_ONE_QUERY, pars, &w, &TeBkUmLpqRouter::dbUtil);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqRouter::dbUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	TeBkUmLpqWorld* w = (TeBkUmLpqWorld*)ctx;
	w->setId(ntohl(*((uint32_t *) data.at(0).d)));
	w->setRandomNumber(ntohl(*((uint32_t *) data.at(1).d)));
}


void TeBkUmLpqRouter::queries(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	LibpqDataSourceImpl* sqli = getDb();

	try {
		TeBkUmLpqWorld w;
		std::vector<LibpqParam> pars;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			pars.clear();
			LibpqDataSourceImpl::ADD_INT4(pars, rid);
			sqli->executeQuery(WORLD_ONE_QUERY, pars, &w, &TeBkUmLpqRouter::dbUtil);
			wlst.push_back(w);
		}
	} catch(const std::exception& e) {
		throw e;
	}
}


void TeBkUmLpqRouter::queriesMulti(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	LibpqDataSourceImpl* sqli = getDb();

	UpdQrData updt;
	updt.wlist = &wlst;
	updt.status = true;

	try {
		std::stringstream ss;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			ss << "select id, randomnumber from world where id = " << rid << ";";
		}
		sqli->executeMultiQuery(ss.str(), &wlst, &TeBkUmLpqRouter::queriesMultiUtil, &TeBkUmLpqRouter::updatesMultiUtilCh);

		if(!updt.status) {
			wlst.clear();
		}

	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqRouter::queriesMultiUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	std::vector<TeBkUmLpqWorld>* wlst = (std::vector<TeBkUmLpqWorld>*)ctx;
	TeBkUmLpqWorld w;
	int tmp = 0;
	strToNum(data.at(0).d, data.at(0).l, tmp);
	w.setId(tmp);
	strToNum(data.at(1).d, data.at(1).l, tmp);
	w.setRandomNumber(tmp);
	wlst->push_back(w);
}


void TeBkUmLpqRouter::updates(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	LibpqDataSourceImpl* sqli = getDb();

	try {
		std::vector<LibpqParam> pars;
		std::vector<LibpqParam> qp;

		std::stringstream ss;
		ss << "update world as t set randomnumber = c.randomnumber from (values";

		int pc = 1;
		for (int c = 0; c < queryCount; ++c) {
			ss << "($";
			ss << pc++;
			ss << "::int4,$";
			ss << pc++;
			if(c!=queryCount-1) {
				ss << "::int4),";
			} else {
				ss << "::int4)";
			}

			int rid = rand() % 10000 + 1;
			qp.clear();
			LibpqDataSourceImpl::ADD_INT4(qp, rid);
			TeBkUmLpqWorld w;
			sqli->executeQuery(WORLD_ONE_QUERY, qp, &w, &TeBkUmLpqRouter::dbUtil);
			wlst.push_back(w);

			LibpqDataSourceImpl::ADD_INT4(pars, w.getId());
			LibpqDataSourceImpl::ADD_INT4(pars, w.getRandomNumber());
		}
		ss << ") as c(id, randomnumber) where c.id = t.id";
		
		sqli->begin();
		sqli->executeUpdateQuery(ss.str(), pars);
		sqli->commit();
	} catch(const std::exception& e) {
		sqli->rollback();
		throw e;
	}
}
void TeBkUmLpqRouter::updatesUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	std::vector<LibpqParam>* pars = (std::vector<LibpqParam>*)ctx;
	int newRandomNumber = rand() % 10000 + 1;
	if((int)ntohl(*((uint32_t *) data.at(1).d)) == newRandomNumber) {
		newRandomNumber += 1;
		if(newRandomNumber>=10000) {
			newRandomNumber = 1;
		}
	}
	uint32_t id = *(uint32_t *)data.at(0).d;
	LibpqDataSourceImpl::ADD_INT4(*pars, id, false);
	LibpqDataSourceImpl::ADD_INT4(*pars, newRandomNumber);
}

void TeBkUmLpqRouter::updatesMulti(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	LibpqDataSourceImpl* sqli = getDb();

	try {
		std::stringstream ss, ssq;
		ss << "begin;update world as t set randomnumber = c.randomnumber from (values";

		UpdQrData updt;
		updt.wlist = &wlst;
		updt.ss = &ss;
		updt.status = true;
		updt.queryCount = queryCount;

		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			ssq << "select id, randomnumber from world where id = " << rid << ";";
		}

		sqli->executeMultiQuery(ssq.str(), &updt, &TeBkUmLpqRouter::updatesMultiUtil, &TeBkUmLpqRouter::updatesMultiUtilCh);
		ss << ") as c(id, randomnumber) where c.id = t.id;commit";

		if(!updt.status) {
			return;
		}

		sqli->executeUpdateMultiQuery(ss.str(), &updt, &TeBkUmLpqRouter::updatesMultiUtilCh);

		if(!updt.status) {
			wlst.clear();
		}
	} catch(const std::exception& e) {
		sqli->rollback();
		throw e;
	}
}
void TeBkUmLpqRouter::updatesMultiUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	UpdQrData* updt = (UpdQrData*)ctx;
	std::stringstream* ss = updt->ss;
	TeBkUmLpqWorld w;
	int tmp = 0;
	strToNum(data.at(0).d, data.at(0).l, tmp);
	w.setId(tmp);
	strToNum(data.at(1).d, data.at(1).l, tmp);
	int newRandomNumber = rand() % 10000 + 1;
	if(tmp == newRandomNumber) {
		newRandomNumber += 1;
		if(newRandomNumber>=10000) {
			newRandomNumber = 1;
		}
	}
	w.setRandomNumber(newRandomNumber);
	updt->wlist->push_back(w);
	*ss << "(" << w.getId() << "," << w.getRandomNumber() << ")";
	updt->queryCount--;
	if(updt->queryCount>0) {
		*ss << ",";
	}
}
void TeBkUmLpqRouter::updatesMultiUtilCh(void* ctx, bool status, std::string query, int counter) {
	UpdQrData* updt = (UpdQrData*)ctx;
	if(!status) {
		updt->status = status;
	}
}


void TeBkUmLpqRouter::updateCache() {
	CacheInterface* cchi = CacheManager::getImpl();
	LibpqDataSourceImpl* sqli = getDb();

	try {
		std::vector<TeBkUmLpqWorld> wlist;
		std::vector<LibpqParam> pars;
		sqli->executeQuery(WORLD_ALL_QUERY, pars, &wlist, &TeBkUmLpqRouter::updateCacheUtil);

		for (int c = 0; c < (int)wlist.size(); ++c) {
			TeBkUmLpqWorld& w = wlist.at(c);
			char str[12];
			sprintf(str, "%d;%d", w.getId(), w.getRandomNumber());
			cchi->setRaw(CastUtil::fromNumber(w.getId()), str);
		}
		CacheManager::cleanImpl(cchi);
	} catch(const std::exception& e) {
		CacheManager::cleanImpl(cchi);
		throw e;
	}
}
void TeBkUmLpqRouter::updateCacheUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	std::vector<TeBkUmLpqWorld>* wlist = (std::vector<TeBkUmLpqWorld>*)ctx;
	TeBkUmLpqWorld w;
	w.setId(ntohl(*((uint32_t *) data.at(0).d)));
	w.setRandomNumber(ntohl(*((uint32_t *) data.at(1).d)));
	wlist->push_back(w);
}

void TeBkUmLpqRouter::cachedWorlds(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	CacheInterface* cchi = CacheManager::getImpl();

	try {
		std::vector<std::string> keys;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			TeBkUmLpqWorld w;
			std::string v = cchi->getValue(CastUtil::fromNumber(rid));
			size_t fn = v.find(";");
			int tmp = 0;
			strToNum(v.substr(0, fn).c_str(), fn, tmp);
			w.setId(tmp);
			tmp = 0;
			strToNum(v.substr(fn+1).c_str(), v.length()-fn-1, tmp);
			w.setRandomNumber(tmp);
			wlst.push_back(w);
		}
		CacheManager::cleanImpl(cchi);
	} catch(const std::exception& e) {
		CacheManager::cleanImpl(cchi);
		throw e;
	}
}

void TeBkUmLpqRouter::getContext(HttpRequest* request, Context* context) {
	LibpqDataSourceImpl* sqli = getDb();

	try {
		std::vector<TeBkUmLpqFortune>* flst = new std::vector<TeBkUmLpqFortune>;
		std::vector<LibpqParam> pars;
		sqli->executeQuery(FORTUNE_ALL_QUERY, pars, flst, &TeBkUmLpqRouter::getContextUtil);

		TeBkUmLpqFortune nf;
		nf.setId(0);
		nf.setMessage("Additional fortune added at request time.");
		flst->push_back(nf);
		std::sort (flst->begin(), flst->end());

		context->insert(std::pair<std::string, void*>("fortunes", flst));
	} catch(...) {
		throw;
	}
}
void TeBkUmLpqRouter::getContextUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	std::vector<TeBkUmLpqFortune>* flst = (std::vector<TeBkUmLpqFortune>*)ctx;
	TeBkUmLpqFortune w;
	w.setId(ntohl(*((uint32_t *) data.at(0).d)));
	std::string nm = std::string(data.at(1).d, data.at(1).l);
	CryptoHandler::sanitizeHtml(nm);
	w.setMessage(nm);
	flst->push_back(w);
}


//https://stackoverflow.com/questions/9631225/convert-strings-specified-by-length-not-nul-terminated-to-int-float
bool TeBkUmLpqRouter::strToNum(const char* str, int len, int& ret) {
    ret = 0;
    for(int i = 0; i < len; ++i)
    {
    	if(!isdigit(str[i])) return false;
        ret = ret * 10 + (str[i] - '0');
    }
    return true;
}

bool TeBkUmLpqRouter::route(HttpRequest* req, HttpResponse* res, void* dlib, void* ddlib, SocketInterface* sif) {
	std::string_view path = req->getPath();
	if(StringUtil::endsWith(path, "/plaintext")) {
		res->setContent(HELLO_WORLD);
		res->setContentType(ContentTypes::CONTENT_TYPE_TEXT_PLAIN);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/json")) {
		TeBkUmLpqMessage msg;
		msg.setMessage(HELLO_WORLD);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 0, "TeBkUmLpqMessage"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/db")) {
		TeBkUmLpqWorld msg;
		db(msg);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 0, "TeBkUmLpqWorld"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/queries_old")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		queries(params[0].val, params[0].val_len, msg);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 100, "std::vector<TeBkUmLpqWorld>"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/queries")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		queriesMulti(params[0].val, params[0].val_len, msg);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 100, "std::vector<TeBkUmLpqWorld>"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/fortunes")) {
		Context ctx;
		getContext(req, &ctx);

		void* mkr = dlsym(ddlib, TPE_FN_NAME.c_str());
		if(mkr!=NULL)
		{
			TeBkUmLpqTemplatePtr f =  (TeBkUmLpqTemplatePtr)mkr;
			std::string msg;
			f(&ctx, msg);
			res->setContent(msg);
			res->setContentType(ContentTypes::CONTENT_TYPE_TEXT_SHTML);
			res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		}
	} else if(StringUtil::endsWith(path, "/updates_old")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		updates(params[0].val, params[0].val_len, msg);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 100, "std::vector<TeBkUmLpqWorld>"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/updates")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		updatesMulti(params[0].val, params[0].val_len, msg);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 100, "std::vector<TeBkUmLpqWorld>"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/cached-worlds")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		cachedWorlds(params[0].val, params[0].val_len, msg);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 100, "std::vector<TeBkUmLpqWorld>"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else {
		res->setHTTPResponseStatus(HTTPResponseStatus::NotFound);
	}
	res->setDone(true);
	return true;
}

std::string TeBkUmLpqRouter::APP_NAME = "";
std::string TeBkUmLpqRouter::TPE_FN_NAME = "";

TeBkUmLpqRouter::TeBkUmLpqRouter() {
	sqli = NULL;
	if(APP_NAME=="") {
		APP_NAME = CommonUtils::normalizeAppName("te-benchmark-um-pq");
		TPE_FN_NAME = CommonUtils::getTpeFnName("tpe/fortunes.tpe", "te-benchmark-um-pq");
	}
}

TeBkUmLpqRouter::~TeBkUmLpqRouter() {
}

LibpqDataSourceImpl* TeBkUmLpqRouter::getDb() {
	if(sqli==NULL) {
		sqli = static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "te-benchmark-um-pq"));
	}
	return sqli;
}
