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
 * TeBkUmLpqAsyncUm.cpp
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */
#include "TeBkUmLpqAsync.h"

int TeBkUmLpqAsyncWorld::getId() const {
	return id;
}

void TeBkUmLpqAsyncWorld::setId(int id) {
	this->id = id;
}

int TeBkUmLpqAsyncWorld::getRandomNumber() const {
	return randomNumber;
}

void TeBkUmLpqAsyncWorld::setRandomNumber(int randomNumber) {
	this->randomNumber = randomNumber;
}

TeBkUmLpqAsyncWorld::TeBkUmLpqAsyncWorld() {
	id = 0;
	randomNumber = 0;
}

TeBkUmLpqAsyncWorld::~TeBkUmLpqAsyncWorld() {
}

int TeBkUmLpqAsyncFortune::getId() const {
	return id;
}

void TeBkUmLpqAsyncFortune::setId(int id) {
	this->id = id;
}

const std::string& TeBkUmLpqAsyncFortune::getMessage() const {
	return message;
}

void TeBkUmLpqAsyncFortune::setMessage(const std::string& message) {
	this->message = message;
}

TeBkUmLpqAsyncFortune::TeBkUmLpqAsyncFortune() {
	id = 0;
}

TeBkUmLpqAsyncFortune::~TeBkUmLpqAsyncFortune() {
}

bool TeBkUmLpqAsyncFortune::operator < (const TeBkUmLpqAsyncFortune& other) const {
	return message.compare(other.message)<0;
}

TeBkUmLpqAsyncMessage::~TeBkUmLpqAsyncMessage() {
}

const std::string& TeBkUmLpqAsyncMessage::getMessage() const {
	return message;
}

void TeBkUmLpqAsyncMessage::setMessage(const std::string& message) {
	this->message = message;
}

const std::string TeBkUmLpqAsyncRouter::HELLO_WORLD = "Hello, World!";
std::string TeBkUmLpqAsyncRouter::WORLD = "world";
std::string TeBkUmLpqAsyncRouter::WORLD_ONE_QUERY = "select id, randomnumber from world where id = $1";
std::string TeBkUmLpqAsyncRouter::WORLD_ALL_QUERY = "select id, randomnumber from world";
std::string TeBkUmLpqAsyncRouter::FORTUNE_ALL_QUERY = "select id, message from fortune";

void TeBkUmLpqAsyncRouter::dbAsync(AsyncReq* req) {
	req->d = new TeBkUmLpqAsyncWorld;
	LibpqDataSourceImpl* sqli = getDb();
	int rid = rand() % 10000 + 1;
	try {
		std::vector<LibpqParam> pars;
		LibpqDataSourceImpl::ADD_INT4(pars, rid);
		void* areq = sqli->executeQueryAsync(WORLD_ONE_QUERY, pars, req, &TeBkUmLpqAsyncRouter::dbAsyncUtil, &TeBkUmLpqAsyncRouter::dbAsyncCh, NULL);
		sqli->completeAsync(areq);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::dbAsyncUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	AsyncReq* req = (AsyncReq*)ctx;
	TeBkUmLpqAsyncWorld* w = (TeBkUmLpqAsyncWorld*)req->d;
	w->setId(ntohl(*((uint32_t *) data.at(0).d)));
	w->setRandomNumber(ntohl(*((uint32_t *) data.at(1).d)));
}
void TeBkUmLpqAsyncRouter::dbAsyncCh(void* ctx, bool status, std::string q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	TeBkUmLpqAsyncWorld* w = (TeBkUmLpqAsyncWorld*)req->d;
	req->r.setContent(JSONSerialize::serializeUnknown(w, 0, "TeBkUmLpqAsyncWorld", APP_NAME));
	req->r.setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	req->r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
	ResponseData d;
	req->r.generateHeadResponse(d._b);
	d._b += req->r.getContent();
	req->sif->writeTo(&d);
	req->sif->unUse();
	delete w;
	delete req;
}


void TeBkUmLpqAsyncRouter::queriesAsync(const char* q, int ql, AsyncReq* req) {
	req->d = new std::vector<TeBkUmLpqAsyncWorld>;

	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	LibpqDataSourceImpl* sqli = getDb();

	try {
		void* areq = NULL;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			std::vector<LibpqParam> pars;
			LibpqDataSourceImpl::ADD_INT4(pars, rid);
			areq = sqli->executeQueryAsync(WORLD_ONE_QUERY, pars, req, &TeBkUmLpqAsyncRouter::queriesAsyncUtil, &TeBkUmLpqAsyncRouter::queriesAsyncCh, areq);
		}
		sqli->completeAsync(areq);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::queriesAsyncUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	AsyncReq* req = (AsyncReq*)ctx;
	std::vector<TeBkUmLpqAsyncWorld>* vec = (std::vector<TeBkUmLpqAsyncWorld>*)req->d;
	TeBkUmLpqAsyncWorld w;
	w.setId(ntohl(*((uint32_t *) data.at(0).d)));
	w.setRandomNumber(ntohl(*((uint32_t *) data.at(1).d)));
	vec->push_back(w);
}
void TeBkUmLpqAsyncRouter::queriesAsyncCh(void* ctx, bool status, std::string q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	std::vector<TeBkUmLpqAsyncWorld>* vec = (std::vector<TeBkUmLpqAsyncWorld>*)req->d;
	req->r.setContent(JSONSerialize::serializeUnknown(vec, 100, "std::vector<TeBkUmLpqAsyncWorld>", APP_NAME));
	req->r.setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	req->r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
	ResponseData d;
	req->r.generateHeadResponse(d._b);
	d._b += req->r.getContent();
	req->sif->writeTo(&d);
	req->sif->unUse();
	delete vec;
	delete req;
}


void TeBkUmLpqAsyncRouter::updatesAsync(const char* q, int ql, AsyncReq* req) {
	req->d = new std::vector<TeBkUmLpqAsyncWorld>;

	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	LibpqDataSourceImpl* sqli = getDb();
	req->sqli = sqli;

	try {
		void* areq = NULL;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			std::vector<LibpqParam> pars;
			LibpqDataSourceImpl::ADD_INT4(pars, rid);
			areq = sqli->executeQueryAsync(WORLD_ONE_QUERY, pars, req, &TeBkUmLpqAsyncRouter::queriesAsyncUtil, &TeBkUmLpqAsyncRouter::updatesAsyncChQ, areq);
		}
		sqli->completeAsync(areq);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::updatesAsyncChQ(void* ctx, bool status, std::string q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	std::vector<TeBkUmLpqAsyncWorld>* vec = (std::vector<TeBkUmLpqAsyncWorld>*)req->d;

	std::stringstream ss;
	ss << "update world as t set randomnumber = c.randomnumber from (values";

	std::vector<LibpqParam> pars;

	int queryCount = (int)vec->size();
	for (int c = 0; c < queryCount; ++c) {
		int newRandomNumber = rand() % 10000 + 1;
		if(vec->at(c).getRandomNumber() == newRandomNumber) {
			newRandomNumber += 1;
			if(newRandomNumber>=10000) {
				newRandomNumber = 1;
			}
		}
		vec->at(c).setRandomNumber(newRandomNumber);
		ss << "(" << vec->at(c).getId() << "," << newRandomNumber << ")";
		if(c!=queryCount-1) {
			ss << ",";
		}
	}
	ss << ") as c(id, randomnumber) where c.id = t.id";

	LibpqDataSourceImpl* sqli = req->sqli;

	AsyncReq* ar = new AsyncReq;
	ar->sif = req->sif;
	ar->r = std::move(req->r);
	ar->d = req->d;
	req->d = NULL;
	req->sif = NULL;

	try {
		void* areq = sqli->beginAsync();
		sqli->executeUpdateQueryAsync(ss.str(), pars, NULL, NULL, areq, false);
		sqli->commitAsync(areq);
		sqli->completeAsync(areq, ar, &TeBkUmLpqAsyncRouter::updatesAsyncChU);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::updatesAsyncChU(void* ctx, bool status, std::string q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	std::vector<TeBkUmLpqAsyncWorld>* vec = (std::vector<TeBkUmLpqAsyncWorld>*)req->d;
	req->r.setContent(JSONSerialize::serializeUnknown(vec, 100, "std::vector<TeBkUmLpqAsyncWorld>", APP_NAME));
	req->r.setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	req->r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
	ResponseData d;
	req->r.generateHeadResponse(d._b);
	d._b += req->r.getContent();
	req->sif->writeTo(&d);
	req->sif->unUse();
	delete vec;
	delete req;
}

void TeBkUmLpqAsyncRouter::updateCache() {
	LibpqDataSourceImpl* sqli = getDb();

	CacheReq* req = new CacheReq;
	req->d = new std::vector<TeBkUmLpqAsyncWorld>;
	req->cchi = CacheManager::getImpl();

	try {
		std::vector<LibpqParam> pars;
		void* areq = sqli->executeQueryAsync(WORLD_ALL_QUERY, pars, req, &TeBkUmLpqAsyncRouter::updateCacheAsyncUtil, &TeBkUmLpqAsyncRouter::updateCacheAsyncCh, NULL);
		sqli->completeAsync(areq);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::updateCacheAsyncUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	CacheReq* req = (CacheReq*)ctx;
	std::vector<TeBkUmLpqAsyncWorld>* wlist = (std::vector<TeBkUmLpqAsyncWorld>*)req->d;
	TeBkUmLpqAsyncWorld w;
	w.setId(ntohl(*((uint32_t *) data.at(0).d)));
	w.setRandomNumber(ntohl(*((uint32_t *) data.at(1).d)));
	wlist->push_back(w);
}
void TeBkUmLpqAsyncRouter::updateCacheAsyncCh(void* ctx, bool status, std::string q, int counter) {
	CacheReq* req = (CacheReq*)ctx;
	std::vector<TeBkUmLpqAsyncWorld>* wlist = (std::vector<TeBkUmLpqAsyncWorld>*)req->d;
	CacheInterface* cchi = req->cchi;

	try {
		for (int c = 0; c < (int)wlist->size(); ++c) {
			TeBkUmLpqAsyncWorld& w = wlist->at(c);
			char str[12];
			sprintf(str, "%d;%d", w.getId(), w.getRandomNumber());
			cchi->setRaw(CastUtil::fromNumber(w.getId()), str);
		}
		delete wlist;
		CacheManager::cleanImpl(cchi);
		delete req;
	} catch(const std::exception& e) {
		delete wlist;
		CacheManager::cleanImpl(cchi);
		delete req;
		throw e;
	}
}

void TeBkUmLpqAsyncRouter::cachedWorlds(const char* q, int ql, std::vector<TeBkUmLpqAsyncWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	CacheInterface* cchi = CacheManager::getImpl();

	try {
		std::vector<std::string> keys;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			TeBkUmLpqAsyncWorld w;
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


void TeBkUmLpqAsyncRouter::getContextAsync(AsyncReq* req) {
	req->d = new std::vector<TeBkUmLpqAsyncFortune>;

	LibpqDataSourceImpl* sqli = getDb();

	try {
		std::vector<LibpqParam> pars;
		void* areq = sqli->executeQueryAsync(FORTUNE_ALL_QUERY, pars, req, &TeBkUmLpqAsyncRouter::getContextAsyncUtil, &TeBkUmLpqAsyncRouter::getContextAsyncCh, NULL);
		sqli->completeAsync(areq);
	} catch(...) {
		throw;
	}
}
void TeBkUmLpqAsyncRouter::getContextAsyncUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	AsyncReq* req = (AsyncReq*)ctx;
	std::vector<TeBkUmLpqAsyncFortune>* flst = (std::vector<TeBkUmLpqAsyncFortune>*)req->d;
	TeBkUmLpqAsyncFortune w;
	w.setId(ntohl(*((uint32_t *) data.at(0).d)));
	std::string nm = std::string(data.at(1).d, data.at(1).l);
	CryptoHandler::sanitizeHtml(nm);
	w.setMessage(nm);
	flst->push_back(w);
}
void TeBkUmLpqAsyncRouter::getContextAsyncCh(void* ctx, bool status, std::string q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	std::vector<TeBkUmLpqAsyncFortune>* flst = (std::vector<TeBkUmLpqAsyncFortune>*)req->d;

	Context context;

	TeBkUmLpqAsyncFortune nf;
	nf.setId(0);
	nf.setMessage("Additional fortune added at request time.");
	flst->push_back(nf);
	std::sort (flst->begin(), flst->end());

	context.insert(std::pair<std::string, void*>("fortunes", flst));

	void* mkr = dlsym(req->ddlib, TPE_FN_NAME.c_str());
	if(mkr!=NULL)
	{
		TeBkUmLpqAsyncTemplatePtr f =  (TeBkUmLpqAsyncTemplatePtr)mkr;
		std::string msg;
		f(&context, msg);
		req->r.setContent(msg);
		req->r.setContentType(ContentTypes::CONTENT_TYPE_TEXT_SHTML);
		req->r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
	}

	ResponseData d;
	req->r.generateHeadResponse(d._b);
	d._b += req->r.getContent();
	req->sif->writeTo(&d);
	req->sif->unUse();
}

//https://stackoverflow.com/questions/9631225/convert-strings-specified-by-length-not-nul-terminated-to-int-float
bool TeBkUmLpqAsyncRouter::strToNum(const char* str, int len, int& ret) {
    ret = 0;
    for(int i = 0; i < len; ++i)
    {
    	if(!isdigit(str[i])) return false;
        ret = ret * 10 + (str[i] - '0');
    }
    return true;
}

bool TeBkUmLpqAsyncRouter::route(HttpRequest* req, HttpResponse* res, void* dlib, void* ddlib, SocketInterface* sif) {
	std::string_view path = req->getPath();
	if(StringUtil::endsWith(path, "/plaintext")) {
		res->setContent(HELLO_WORLD);
		res->setContentType(ContentTypes::CONTENT_TYPE_TEXT_PLAIN);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/json")) {
		TeBkUmLpqAsyncMessage msg;
		msg.setMessage(HELLO_WORLD);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 0, "TeBkUmLpqAsyncMessage"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/db")) {
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		sif->use();
		ar->r.addHeader(HttpResponse::DateHeader, res->getHeader(HttpResponse::DateHeader));
		ar->r.update(req);
		if(req->isClose()) {
			ar->r.addHeader(HttpResponse::Connection, "close");
		} else if(req->getHttpVers()>=1.1) {
			ar->r.addHeader(HttpResponse::Connection, "keep-alive");
		}
		dbAsync(ar);
		return false;
	} else if(StringUtil::endsWith(path, "/queries")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		sif->use();
		ar->r.addHeader(HttpResponse::DateHeader, res->getHeader(HttpResponse::DateHeader));
		ar->r.update(req);
		if(req->isClose()) {
			ar->r.addHeader(HttpResponse::Connection, "close");
		} else if(req->getHttpVers()>=1.1) {
			ar->r.addHeader(HttpResponse::Connection, "keep-alive");
		}
		queriesAsync(params[0].val, params[0].val_len, ar);
		return false;
	} else if(StringUtil::endsWith(path, "/fortunes")) {
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		sif->use();
		ar->ddlib = ddlib;
		ar->r.addHeader(HttpResponse::DateHeader, res->getHeader(HttpResponse::DateHeader));
		ar->r.update(req);
		if(req->isClose()) {
			ar->r.addHeader(HttpResponse::Connection, "close");
		} else if(req->getHttpVers()>=1.1) {
			ar->r.addHeader(HttpResponse::Connection, "keep-alive");
		}
		getContextAsync(ar);
		return false;
	} else if(StringUtil::endsWith(path, "/updates")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		sif->use();
		ar->r.addHeader(HttpResponse::DateHeader, res->getHeader(HttpResponse::DateHeader));
		ar->r.update(req);
		if(req->isClose()) {
			ar->r.addHeader(HttpResponse::Connection, "close");
		} else if(req->getHttpVers()>=1.1) {
			ar->r.addHeader(HttpResponse::Connection, "keep-alive");
		}
		updatesAsync(params[0].val, params[0].val_len, ar);
		return false;
	} else if(StringUtil::endsWith(path, "/cached-worlds")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqAsyncWorld> msg;
		cachedWorlds(params[0].val, params[0].val_len, msg);
		res->setContent(JSONSerialize::serializeUnknown(&msg, 100, "std::vector<TeBkUmLpqAsyncWorld>"));
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else {
		res->setHTTPResponseStatus(HTTPResponseStatus::NotFound);
	}
	res->setDone(true);
	return true;
}

std::string TeBkUmLpqAsyncRouter::APP_NAME = "";
std::string TeBkUmLpqAsyncRouter::TPE_FN_NAME = "";

TeBkUmLpqAsyncRouter::TeBkUmLpqAsyncRouter() {
	sqli = NULL;
	if(APP_NAME=="") {
		APP_NAME = CommonUtils::normalizeAppName("te-benchmark-um-pq-async");
		TPE_FN_NAME = CommonUtils::getTpeFnName("tpe/fortunes.tpe", "te-benchmark-um-pq-async");
	}
}

TeBkUmLpqAsyncRouter::~TeBkUmLpqAsyncRouter() {
}

LibpqDataSourceImpl* TeBkUmLpqAsyncRouter::getDb() {
	if(sqli==NULL) {
		sqli = static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "te-benchmark-um-pq-async"));
	}
	return sqli;
}
