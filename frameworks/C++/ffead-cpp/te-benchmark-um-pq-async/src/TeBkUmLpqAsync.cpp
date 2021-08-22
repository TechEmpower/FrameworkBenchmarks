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

TeBkUmLpqAsyncWorld::TeBkUmLpqAsyncWorld(int id, int randomNumber) {
	this->id = id;
	this->randomNumber = randomNumber;
}

TeBkUmLpqAsyncWorld::TeBkUmLpqAsyncWorld(int id) {
	this->id = id;
	randomNumber = 0;
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

TeBkUmLpqAsyncFortune::TeBkUmLpqAsyncFortune(int id) {
	this->id = id;
	allocd = false;
}

TeBkUmLpqAsyncFortune::TeBkUmLpqAsyncFortune(int id, std::string message) {
	this->id = id;
	this->message_i = message;
	this->message = std::string_view(this->message_i);
	allocd = false;
}

TeBkUmLpqAsyncFortune::TeBkUmLpqAsyncFortune() {
	id = 0;
	allocd = false;
}

TeBkUmLpqAsyncFortune::~TeBkUmLpqAsyncFortune() {
	if(allocd && message.size()>0) {
		free((void *)message.data());
	}
}

bool TeBkUmLpqAsyncFortune::operator < (const TeBkUmLpqAsyncFortune& other) const {
	return message.compare(other.message)<0;
}

TeBkUmLpqAsyncMessage::TeBkUmLpqAsyncMessage() {
}

TeBkUmLpqAsyncMessage::TeBkUmLpqAsyncMessage(std::string message) {
	this->message = message;
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
std::map<int, std::string> TeBkUmLpqAsyncRouter::_qC;

void TeBkUmLpqAsyncRouter::dbAsync(AsyncReq* req) {
	LibpqDataSourceImpl* sqli = getDb();
	int rid = rand() % 10000 + 1;
	try {
		std::vector<LibpqParam> pars;
		LibpqDataSourceImpl::ADD_INT4(pars, rid);
		void* areq = sqli->executeQueryAsync(WORLD_ONE_QUERY, std::move(pars), req, &TeBkUmLpqAsyncRouter::dbAsyncUtil, &TeBkUmLpqAsyncRouter::dbAsyncCh, NULL);
		sqli->completeAsync(areq);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::dbAsyncUtil(void* ctx, int rn, int cn, char * d) {
	AsyncReq* req = (AsyncReq*)ctx;
	if(cn==0)req->w.setId(ntohl(*((uint32_t *) d)));
	if(cn==1)req->w.setRandomNumber(ntohl(*((uint32_t *) d)));
}
void TeBkUmLpqAsyncRouter::dbAsyncCh(void* ctx, bool status, const std::string& q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	HttpResponse r;
	r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
	JSONSerialize::serializeObject(&req->w, w_ser, r.getContentP());
	std::string d;
	r.generateHeadResponse(d, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->httpVers, req->conn_clos);
	req->sif->writeDirect(d);
	req->sif->writeDirect(r.getContent());
	req->sif->unUse();
	delete req;
}


void TeBkUmLpqAsyncRouter::queriesAsync(const char* q, int ql, AsyncReq* req) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	req->vec.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	try {
		void* areq = NULL;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			std::vector<LibpqParam> pars;
			LibpqDataSourceImpl::ADD_INT4(pars, rid);
			areq = sqli->executeQueryAsync(WORLD_ONE_QUERY, std::move(pars), req, &TeBkUmLpqAsyncRouter::queriesAsyncUtil, &TeBkUmLpqAsyncRouter::queriesAsyncCh, areq);
		}
		sqli->completeAsync(areq);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::queriesAsyncUtil(void* ctx, int rn, int cn, char * d) {
	AsyncReq* req = (AsyncReq*)ctx;
	if(cn==0) {
		req->vec.emplace_back(ntohl(*((uint32_t *) d)));
	} else {
		req->vec.back().setRandomNumber(ntohl(*((uint32_t *) d)));
	}
}
void TeBkUmLpqAsyncRouter::queriesAsyncCh(void* ctx, bool status, const std::string& q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	HttpResponse r;
	r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
	JSONSerialize::serializeObjectCont(&req->vec, wcont_ser, "vector", r.getContentP());
	std::string d;
	r.generateHeadResponse(d, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->httpVers, req->conn_clos);
	req->sif->writeDirect(d);
	req->sif->writeDirect(r.getContent());
	req->sif->unUse();
	delete req;
}


#ifndef HAVE_LIBPQ_BATCH
void TeBkUmLpqAsyncRouter::queriesMultiAsync(const char* q, int ql, AsyncReq* req) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	req->vec.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	try {
		std::stringstream ss;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			ss << "select id, randomnumber from world where id = " << rid << ";";
		}
		void* areq = sqli->executeMultiQueryAsync(ss.str(), req, &TeBkUmLpqAsyncRouter::queriesMultiAsyncUtil, &TeBkUmLpqAsyncRouter::queriesMultiAsyncCh);
		sqli->completeAsync(areq, queryCount);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::queriesMultiAsyncUtil(void* ctx, int rn, int cn, char * d, int l) {
	AsyncReq* req = (AsyncReq*)ctx;
	int tmp = 0;
	strToNum(d, l, tmp);
	if(cn==0) {
		req->vec.emplace_back(tmp);
	} else {
		req->vec.back().setRandomNumber(tmp);
	}
}
void TeBkUmLpqAsyncRouter::queriesMultiAsyncCh(void* ctx, bool status, const std::string& q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	HttpResponse r;
	r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
	JSONSerialize::serializeObjectCont(&req->vec, wcont_ser, "vector", r.getContentP());
	std::string d;
	r.generateHeadResponse(d, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->httpVers, req->conn_clos);
	req->sif->writeDirect(d);
	req->sif->writeDirect(r.getContent());
	req->sif->unUse();
	delete req;
}
#endif



std::string& TeBkUmLpqAsyncRouter::getUpdQuery(int count) {
	std::map<int, std::string>::iterator it = _qC.find(count);
	if(it!=_qC.end()) {
		return it->second;
	}

	std::stringstream ss;
	ss << "update world as t set randomnumber = case id ";

	int pc = 1;
	for (int c = 0; c < count; ++c) {
		ss << "when $";
		ss << pc++;
		ss << " then $";
		ss << pc++;
	}
	ss << "else randomnumber end where id in (";
	for (int c = 0; c < count; ++c) {
		ss << "$" << pc++ << ",";
	}
	std::string q = ss.str();
	q = q.substr(0, q.length()-1);
	q += ")";
	_qC[count] = std::move(q);
	return _qC[count];
}

void TeBkUmLpqAsyncRouter::updatesAsyncb(const char* q, int ql, AsyncReq* req) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	req->vec.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();
	req->sqli = sqli;

	try {
		void* areq = NULL;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			std::vector<LibpqParam> pars;
			LibpqDataSourceImpl::ADD_INT4(pars, rid);
			areq = sqli->executeQueryAsync(WORLD_ONE_QUERY, std::move(pars), req, &TeBkUmLpqAsyncRouter::queriesAsyncUtil, &TeBkUmLpqAsyncRouter::updatesAsyncbChQ, areq);
		}
		sqli->completeAsync(areq);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::updatesAsyncbChQ(void* ctx, bool status, const std::string& q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;

	LibpqDataSourceImpl* sqli = req->sqli;

	int queryCount = (int)req->vec.size();
	std::vector<LibpqParam> pars;

	for(std::vector<TeBkUmLpqAsyncWorld>::iterator it=req->vec.begin(); it != req->vec.end(); ++it) {
		LibpqDataSourceImpl::ADD_INT4(pars, (*it).getId());

		int newRandomNumber = rand() % 10000 + 1;
		if((*it).getRandomNumber() == newRandomNumber) {
			newRandomNumber += 1;
			if(newRandomNumber>=10000) {
				newRandomNumber = 1;
			}
		}
		LibpqDataSourceImpl::ADD_INT4(pars, newRandomNumber);
		(*it).setRandomNumber(newRandomNumber);
	}
	for(std::vector<TeBkUmLpqAsyncWorld>::iterator it=req->vec.begin(); it != req->vec.end(); ++it) {
		LibpqDataSourceImpl::ADD_INT4(pars, (*it).getId());
	}

	void* areq = sqli->beginAsync(NULL);
	sqli->executeUpdateQueryAsync(getUpdQuery(queryCount), std::move(pars), NULL, NULL, areq, true);
	sqli->commitAsync(areq);

	AsyncReq* ar = new AsyncReq;
	ar->sif = req->sif;
	ar->httpVers = req->httpVers;
	ar->conn_clos = req->conn_clos;
	ar->vec = std::move(req->vec);
	req->sif = NULL;

	try {
		sqli->completeAsync(areq, ar, &TeBkUmLpqAsyncRouter::updatesAsyncbChU);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::updatesAsyncbChU(void* ctx, bool status, const std::string& q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	HttpResponse r;
	r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
	JSONSerialize::serializeObjectCont(&req->vec, wcont_ser, "vector", r.getContentP());
	std::string d;
	r.generateHeadResponse(d, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->httpVers, req->conn_clos);
	req->sif->writeDirect(d);
	req->sif->writeDirect(r.getContent());
	req->sif->unUse();
	delete req;
}

void TeBkUmLpqAsyncRouter::updatesAsync(const char* q, int ql, AsyncReq* req) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	req->vec.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();
	req->sqli = sqli;

	try {
		void* areq = NULL;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			std::vector<LibpqParam> pars;
			LibpqDataSourceImpl::ADD_INT4(pars, rid);
			areq = sqli->executeQueryAsync(WORLD_ONE_QUERY, std::move(pars), req, &TeBkUmLpqAsyncRouter::queriesAsyncUtil, &TeBkUmLpqAsyncRouter::updatesAsyncChQ, areq);
		}
		sqli->completeAsync(areq);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::updatesAsyncChQ(void* ctx, bool status, const std::string& q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;

	std::stringstream ss;
	//ss << "update world as t set randomnumber = c.randomnumber from (values";

	LibpqDataSourceImpl* sqli = req->sqli;

	void* areq = NULL;
	for(std::vector<TeBkUmLpqAsyncWorld>::iterator it=req->vec.begin(); it != req->vec.end(); ++it) {
		int newRandomNumber = rand() % 10000 + 1;
		if((*it).getRandomNumber() == newRandomNumber) {
			newRandomNumber += 1;
			if(newRandomNumber>=10000) {
				newRandomNumber = 1;
			}
		}
		(*it).setRandomNumber(newRandomNumber);
		if(areq==NULL) {
			areq = sqli->beginAsync(areq);
		} else {
			sqli->beginAsync(areq);
		}
		ss.str(std::string());
		std::vector<LibpqParam> pars;
		ss << "update world set randomnumber = " << newRandomNumber << " where id = " << (*it).getId();
		sqli->executeUpdateQueryAsync(ss.str(), std::move(pars), NULL, NULL, areq, false);
		sqli->commitAsync(areq);
		/*if(c!=queryCount-1) {
			ss << ",";
		}*/
	}
	//ss << ") as c(id, randomnumber) where c.id = t.id";

	AsyncReq* ar = new AsyncReq;
	ar->sif = req->sif;
	ar->httpVers = req->httpVers;
	ar->conn_clos = req->conn_clos;
	ar->vec = std::move(req->vec);
	req->sif = NULL;

	try {
		sqli->completeAsync(areq, ar, &TeBkUmLpqAsyncRouter::updatesAsyncChU);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::updatesAsyncChU(void* ctx, bool status, const std::string& q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	HttpResponse r;
	r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
	JSONSerialize::serializeObjectCont(&req->vec, wcont_ser, "vector", r.getContentP());
	std::string d;
	r.generateHeadResponse(d, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->httpVers, req->conn_clos);
	req->sif->writeDirect(d);
	req->sif->writeDirect(r.getContent());
	req->sif->unUse();
	delete req;
}

void TeBkUmLpqAsyncRouter::updateCache() {
	LibpqDataSourceImpl* sqli = getDb();

	CacheReq* req = new CacheReq;
	req->cchi = CacheManager::getImpl();

	try {
		std::vector<LibpqParam> pars;
		void* areq = sqli->executeQueryAsync(WORLD_ALL_QUERY, std::move(pars), req, &TeBkUmLpqAsyncRouter::updateCacheAsyncUtil, &TeBkUmLpqAsyncRouter::updateCacheAsyncCh, NULL);
		sqli->completeAsync(areq);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqAsyncRouter::updateCacheAsyncUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	CacheReq* req = (CacheReq*)ctx;
	req->vec.emplace_back(ntohl(*((uint32_t *) data.at(0).d)), ntohl(*((uint32_t *) data.at(1).d)));
}
void TeBkUmLpqAsyncRouter::updateCacheAsyncCh(void* ctx, bool status, const std::string& q, int counter) {
	CacheReq* req = (CacheReq*)ctx;
	CacheInterface* cchi = req->cchi;

	try {
		for(std::vector<TeBkUmLpqAsyncWorld>::iterator it=req->vec.begin(); it != req->vec.end(); ++it) {
			char str[12];
			sprintf(str, "%d;%d", (*it).getId(), (*it).getRandomNumber());
			cchi->setRaw(CastUtil::fromNumber((*it).getId()), str);
		}
		CacheManager::cleanImpl(cchi);
		delete req;
		CacheManager::triggerAppInitCompletion("te-benchmark-um-pq-async");
	} catch(const std::exception& e) {
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

	wlst.reserve(queryCount);

	CacheInterface* cchi = CacheManager::getImpl();

	try {
		std::vector<std::string> keys;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			std::string v = cchi->getValue(CastUtil::fromNumber(rid));
			size_t fn = v.find(";");
			int tmp = 0;
			strToNum(v.substr(0, fn).c_str(), fn, tmp);
			int tmp1 = 0;
			strToNum(v.substr(fn+1).c_str(), v.length()-fn-1, tmp1);
			wlst.emplace_back(tmp, tmp1);
		}
		CacheManager::cleanImpl(cchi);
	} catch(const std::exception& e) {
		CacheManager::cleanImpl(cchi);
		throw e;
	}
}


void TeBkUmLpqAsyncRouter::getContextAsync(AsyncReq* req) {
	LibpqDataSourceImpl* sqli = getDb();

	try {
		std::vector<LibpqParam> pars;
		void* areq = sqli->executeQueryAsync(FORTUNE_ALL_QUERY, std::move(pars), req, &TeBkUmLpqAsyncRouter::getContextAsyncUtil, &TeBkUmLpqAsyncRouter::getContextAsyncCh, NULL);
		sqli->completeAsync(areq);
	} catch(...) {
		throw;
	}
}
void TeBkUmLpqAsyncRouter::getContextAsyncUtil(void* ctx, int rn, int cn, char * d, int l) {
	AsyncReq* req = (AsyncReq*)ctx;
	if(cn==0) {
		req->flst.emplace_back(ntohl(*((uint32_t *) d)));
	} else {
		TeBkUmLpqAsyncFortune& w = req->flst.back();
		w.message = CryptoHandler::sanitizeHtmlFast((const uint8_t *)d, (size_t)l, w.message_i, w.allocd);
	}
}

void TeBkUmLpqAsyncRouter::getContextAsyncCh(void* ctx, bool status, const std::string& q, int counter) {
	AsyncReq* req = (AsyncReq*)ctx;
	Context context;

	req->flst.emplace_back(0, "Additional fortune added at request time.");
	req->flst.sort();

	context.insert(std::pair<std::string, void*>("fortunes", &req->flst));

	if(tmplFunc!=NULL)
	{
		fcpstream str;
		tmplFunc(&context, str);
		HttpResponse r;
		r.setHTTPResponseStatus(HTTPResponseStatus::Ok);
		std::string d;
		r.generateHeadResponse(d, ContentTypes::CONTENT_TYPE_TEXT_SHTML, req->httpVers, req->conn_clos, (int)str.str().length());
		req->sif->writeDirect(d);
		req->sif->writeDirect(str.str());
		req->sif->unUse();
	}
	else
	{
		ResponseData d;
		HttpResponse r;
		r.generateHeadResponse(d._b, req->httpVers, req->conn_clos);
		req->sif->writeTo(&d);
		req->sif->unUse();
	}
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

bool TeBkUmLpqAsyncRouter::route(HttpRequest* req, HttpResponse* res, SocketInterface* sif) {
	sif->use();
	if(StringUtil::endsWith(req->getPath(), "/plaintext")) {
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		std::string d;
		res->generateHeadResponse(d, ContentTypes::CONTENT_TYPE_TEXT_PLAIN, (int)HELLO_WORLD.length());
		sif->writeDirect(d);
		sif->writeDirect(HELLO_WORLD);
		sif->unUse();
	} else if(StringUtil::endsWith(req->getPath(), "/json")) {
		TeBkUmLpqAsyncMessage msg;
		msg.setMessage(HELLO_WORLD);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		JSONSerialize::serializeObject(&msg, m_ser, res->getContentP());
		std::string d;
		res->generateHeadResponse(d, ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		sif->writeDirect(d);
		sif->writeDirect(res->getContent());
		sif->unUse();
	} else if(StringUtil::endsWith(req->getPath(), "/db")) {
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		ar->httpVers = req->getHttpVers();
		ar->conn_clos = req->isClose();
		dbAsync(ar);
	} else if(StringUtil::endsWith(req->getPath(), "/queries")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		ar->httpVers = req->getHttpVers();
		ar->conn_clos = req->isClose();
		queriesAsync(params[0].val, params[0].val_len, ar);
	}
#ifndef HAVE_LIBPQ_BATCH
	else if(StringUtil::endsWith(req->getPath(), "/queriem")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		ar->sif = sif;
		ar->httpVers = req->getHttpVers();
		ar->conn_clos = req->isClose();
		queriesMultiAsync(params[0].val, params[0].val_len, ar);
	}
#endif
	else if(StringUtil::endsWith(req->getPath(), "/fortunes")) {
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		ar->sif = sif;
		ar->httpVers = req->getHttpVers();
		ar->conn_clos = req->isClose();
		getContextAsync(ar);
	} else if(StringUtil::endsWith(req->getPath(), "/bupdates")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		ar->sif = sif;
		ar->httpVers = req->getHttpVers();
		ar->conn_clos = req->isClose();
		updatesAsyncb(params[0].val, params[0].val_len, ar);
	} else if(StringUtil::endsWith(req->getPath(), "/updates")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncReq* ar = new AsyncReq;
		ar->sif = sif;
		ar->sif = sif;
		ar->httpVers = req->getHttpVers();
		ar->conn_clos = req->isClose();
		updatesAsync(params[0].val, params[0].val_len, ar);
	} else if(StringUtil::endsWith(req->getPath(), "/cached-worlds")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqAsyncWorld> msg;
		cachedWorlds(params[0].val, params[0].val_len, msg);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		std::string d;
		res->generateHeadResponse(d, ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		sif->writeDirect(d);
		sif->writeDirect(res->getContent());
		sif->unUse();
	} else {
		res->setHTTPResponseStatus(HTTPResponseStatus::NotFound);
		std::string d;
		res->generateHeadResponse(d, ContentTypes::CONTENT_TYPE_TEXT_PLAIN);
		sif->writeDirect(d);
		sif->unUse();
	}
	return false;
}

TemplatePtr TeBkUmLpqAsyncRouter::tmplFunc;
Ser TeBkUmLpqAsyncRouter::m_ser;
Ser TeBkUmLpqAsyncRouter::w_ser;
SerCont TeBkUmLpqAsyncRouter::wcont_ser;

TeBkUmLpqAsyncRouter::TeBkUmLpqAsyncRouter() {
	sqli = NULL;
	tmplFunc = TemplateUtil::getTemplateFunc("te-benchmark-um-pq-async", "tpe/fortunes.tpe");
	m_ser = Serializer::getSerFuncForObject("te-benchmark-um-pq-async", "TeBkUmLpqAsyncMessage");
	w_ser = Serializer::getSerFuncForObject("te-benchmark-um-pq-async", "TeBkUmLpqAsyncWorld");
	wcont_ser = Serializer::getSerFuncForObjectCont("te-benchmark-um-pq-async", "TeBkUmLpqAsyncWorld", "std::vector");
}

TeBkUmLpqAsyncRouter::~TeBkUmLpqAsyncRouter() {
}

LibpqDataSourceImpl* TeBkUmLpqAsyncRouter::getDb() {
	if(sqli==NULL) {
		sqli = static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "te-benchmark-um-pq-async"));
	}
	return sqli;
}
