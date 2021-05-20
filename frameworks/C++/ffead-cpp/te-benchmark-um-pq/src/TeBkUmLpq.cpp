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

TeBkUmLpqWorld::TeBkUmLpqWorld(int id) {
	this->id = id;
	randomNumber = 0;
}

TeBkUmLpqWorld::TeBkUmLpqWorld(int id, int randomNumber) {
	this->id = id;
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

TeBkUmLpqFortune::TeBkUmLpqFortune(int id) {
	this->id = id;
	allocd = false;
}

TeBkUmLpqFortune::TeBkUmLpqFortune(int id, std::string message) {
	this->id = id;
	this->message_i = message;
	this->message = std::string_view(this->message_i);
	allocd = false;
}

TeBkUmLpqFortune::TeBkUmLpqFortune() {
	id = 0;
	allocd = false;
}

TeBkUmLpqFortune::~TeBkUmLpqFortune() {
	if(allocd && message.size()>0) {
		free((void *)message.data());
	}
}

bool TeBkUmLpqFortune::operator < (const TeBkUmLpqFortune& other) const {
	return message.compare(other.message)<0;
}

TeBkUmLpqMessage::TeBkUmLpqMessage() {
}

TeBkUmLpqMessage::TeBkUmLpqMessage(std::string message) {
	this->message = message;
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
		LibpqParams<1> pars;
		pars.int4(rid);
		sqli->executeQuery(WORLD_ONE_QUERY, &pars, &w, &TeBkUmLpqRouter::dbUtil);
	} catch(const std::exception& e) {
		throw e;
	}
}
void TeBkUmLpqRouter::dbUtil(void* ctx, int rn, int cn, char * d) {
	TeBkUmLpqWorld* w = (TeBkUmLpqWorld*)ctx;
	if(cn==0)w->setId(ntohl(*((uint32_t *) d)));
	if(cn==1)w->setRandomNumber(ntohl(*((uint32_t *) d)));
}

void TeBkUmLpqRouter::queries(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	LibpqParamsBase* pars = sqli->getParams(queryCount);
	try {
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			pars->int4(rid);
			wlst.emplace_back();
			TeBkUmLpqWorld& w = wlst.back();
			sqli->executeQuery(WORLD_ONE_QUERY, pars, &w, &TeBkUmLpqRouter::dbUtil);
		}
		delete pars;
	} catch(const std::exception& e) {
		delete pars;
		throw e;
	}
}


void TeBkUmLpqRouter::queriesMulti(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	wlst.reserve(queryCount);

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
void TeBkUmLpqRouter::queriesMultiUtil(void* ctx, int rn, int cn, char * d, int l) {
	std::vector<TeBkUmLpqWorld>* wlst = (std::vector<TeBkUmLpqWorld>*)ctx;
	int tmp = 0;
	strToNum(d, l, tmp);
	if(cn==0) {
		wlst->emplace_back(tmp);
	} else {
		wlst->back().setRandomNumber(tmp);
	}
}

std::string& TeBkUmLpqRouter::getUpdQuery(int count) {
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

void TeBkUmLpqRouter::updates(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	LibpqParamsBase* pars = sqli->getParams(queryCount*3);
	try {
		std::string& query = getUpdQuery(queryCount);

		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			LibpqParams<1> qp;
			qp.int4(rid);
			wlst.emplace_back();
			TeBkUmLpqWorld& w = wlst.back();
			sqli->executeQuery(WORLD_ONE_QUERY, &qp, &w, &TeBkUmLpqRouter::dbUtil);

			pars->int4(w.getId());

			int newRandomNumber = rand() % 10000 + 1;
			if(w.getRandomNumber() == newRandomNumber) {
				newRandomNumber += 1;
				if(newRandomNumber>=10000) {
					newRandomNumber = 1;
				}
			}
			pars->int4(newRandomNumber);
			w.setRandomNumber(newRandomNumber);
		}
		for(std::vector<TeBkUmLpqWorld>::iterator it=wlst.begin(); it != wlst.end(); ++it) {
			pars->int4((*it).getId());
		}
		
		sqli->begin();
		sqli->executeUpdateQuery(query, pars);
		sqli->commit();
		delete pars;
	} catch(const std::exception& e) {
		delete pars;
		sqli->rollback();
		throw e;
	}
}

void TeBkUmLpqRouter::updatesMulti(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	try {
		std::stringstream ss, ssq;
		//ss << "begin;update world as t set randomnumber = c.randomnumber from (values";

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
		//ss << ") as c(id, randomnumber) where c.id = t.id;commit";

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
void TeBkUmLpqRouter::updatesMultiUtil(void* ctx, int rn, int cn, char * d, int l) {
	UpdQrData* updt = (UpdQrData*)ctx;
	std::stringstream* ss = updt->ss;
	int tmp = 0;
	strToNum(d, l, tmp);
	if(cn==0) {
		updt->wlist->emplace_back(tmp);
	} else {
		TeBkUmLpqWorld& w = updt->wlist->back();
		int newRandomNumber = rand() % 10000 + 1;
		if(tmp == newRandomNumber) {
			newRandomNumber += 1;
			if(newRandomNumber>=10000) {
				newRandomNumber = 1;
			}
		}
		w.setRandomNumber(newRandomNumber);
		*ss << "begin;update world set randomnumber = " << newRandomNumber << " where id = " << w.getId() << ";commit;";
	}
}
void TeBkUmLpqRouter::updatesMultiUtilCh(void* ctx, bool status, const std::string& query, int counter) {
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
		sqli->executeQuery(WORLD_ALL_QUERY, NULL, &wlist, &TeBkUmLpqRouter::updateCacheUtil);

		for(std::vector<TeBkUmLpqWorld>::iterator it=wlist.begin(); it != wlist.end(); ++it) {
			char str[12];
			sprintf(str, "%d;%d", (*it).getId(), (*it).getRandomNumber());
			cchi->setRaw(CastUtil::fromNumber((*it).getId()), str);
		}
		CacheManager::cleanImpl(cchi);
		CacheManager::triggerAppInitCompletion();
	} catch(const std::exception& e) {
		CacheManager::cleanImpl(cchi);
		throw e;
	}
}
void TeBkUmLpqRouter::updateCacheUtil(void* ctx, int rn, std::vector<LibpqRes>& data) {
	std::vector<TeBkUmLpqWorld>* wlist = (std::vector<TeBkUmLpqWorld>*)ctx;
	wlist->emplace_back(ntohl(*((uint32_t *) data.at(0).d)), ntohl(*((uint32_t *) data.at(1).d)));
}

void TeBkUmLpqRouter::cachedWorlds(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;

	wlst.reserve(queryCount);

	CacheInterface* cchi = CacheManager::getImpl();

	try {
		std::vector<std::string> keys, values;
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			keys.emplace_back(CastUtil::fromNumber(rid));
		}
		cchi->mgetRaw(keys, values);
		for (int c = 0; c < queryCount; ++c) {
			std::string& v = values.at(c);
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

void TeBkUmLpqRouter::handleTemplate(HttpResponse* res) {
	LibpqDataSourceImpl* sqli = getDb();

	try {
		Context ctx;
		std::list<TeBkUmLpqFortune> flst;
		sqli->executeQuery(FORTUNE_ALL_QUERY, NULL, &flst, &TeBkUmLpqRouter::getContextUtil);

		flst.emplace_back(0, "Additional fortune added at request time.");
		flst.sort();

		ctx.insert(std::pair<std::string, void*>("fortunes", &flst));

		fcpstream str;
		tmplFunc(&ctx, str);
		res->setContent(str.str());
		res->setContentType(ContentTypes::CONTENT_TYPE_TEXT_SHTML);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} catch(...) {
		throw;
	}
}
void TeBkUmLpqRouter::getContextUtil(void* ctx, int rn, int cn, char * d, int l) {
	std::list<TeBkUmLpqFortune>* flst = (std::list<TeBkUmLpqFortune>*)ctx;
	if(cn==0) {
		flst->emplace_back(ntohl(*((uint32_t *) d)));
	} else {
		TeBkUmLpqFortune& w = flst->back();
		w.message = CryptoHandler::sanitizeHtmlFast((const uint8_t *)d, (size_t)l, w.message_i, w.allocd);
	}
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

bool TeBkUmLpqRouter::route(HttpRequest* req, HttpResponse* res, SocketInterface* sif) {
	if(StringUtil::endsWith(req->getPath(), "/plaintext")) {
		res->setContent(HELLO_WORLD);
		res->setContentType(ContentTypes::CONTENT_TYPE_TEXT_PLAIN);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(req->getPath(), "/json")) {
		TeBkUmLpqMessage msg;
		msg.setMessage(HELLO_WORLD);
		JSONSerialize::serializeObject(&msg, m_ser, res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(req->getPath(), "/db")) {
		TeBkUmLpqWorld msg;
		db(msg);
		JSONSerialize::serializeObject(&msg, w_ser, res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(req->getPath(), "/queries_old")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		queries(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(req->getPath(), "/queries")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		queriesMulti(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(req->getPath(), "/fortunes")) {
		handleTemplate(res);
	} else if(StringUtil::endsWith(req->getPath(), "/bupdates")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		updates(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(req->getPath(), "/updates")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		updatesMulti(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(req->getPath(), "/cached-worlds")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		cachedWorlds(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else {
		res->setHTTPResponseStatus(HTTPResponseStatus::NotFound);
	}
	res->setDone(true);
	return true;
}

TemplatePtr TeBkUmLpqRouter::tmplFunc;
Ser TeBkUmLpqRouter::m_ser;
Ser TeBkUmLpqRouter::w_ser;
SerCont TeBkUmLpqRouter::wcont_ser;

TeBkUmLpqRouter::TeBkUmLpqRouter() {
	sqli = NULL;
	tmplFunc = TemplateUtil::getTemplateFunc("te-benchmark-um-pq", "tpe/fortunes.tpe");
	m_ser = Serializer::getSerFuncForObject("te-benchmark-um-pq", "TeBkUmLpqMessage");
	w_ser = Serializer::getSerFuncForObject("te-benchmark-um-pq", "TeBkUmLpqWorld");
	wcont_ser = Serializer::getSerFuncForObjectCont("te-benchmark-um-pq", "TeBkUmLpqWorld", "std::vector");
}

TeBkUmLpqRouter::~TeBkUmLpqRouter() {
}

LibpqDataSourceImpl* TeBkUmLpqRouter::getDb() {
	if(sqli==NULL) {
		sqli = static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "te-benchmark-um-pq"));
	}
	return sqli;
}
