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

bool TeBkUmLpqWorld::operator < (const TeBkUmLpqWorld& other) const {
	return id < other.id;
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
const std::string TeBkUmLpqRouter::WORLD = "world";
const std::string TeBkUmLpqRouter::WORLD_ONE_QUERY = "select id,randomnumber from world where id=$1";
const std::string TeBkUmLpqRouter::WORLD_ALL_QUERY = "select id,randomnumber from world";
const std::string TeBkUmLpqRouter::FORTUNE_ALL_QUERY = "select id,message from fortune";
int TeBkUmLpqRouter::g_seed = 0;

void TeBkUmLpqRouter::db(TeBkUmLpqWorld& w) {
	LibpqDataSourceImpl* sqli = getDb();
	int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
	LibpqQuery q;
	q.withParamInt4(rid);
#ifdef HAVE_LIBPQ
	q.withSelectQuery(WORLD_ONE_QUERY).withContext(&w).withCb0([](void* ctx, PGresult* res) {
		TeBkUmLpqWorld* w = (TeBkUmLpqWorld*)ctx;
		int cols = PQnfields(res);
		for (int j = 0; j < cols; ++j) {
			if(j==0)w->setId(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
			else w->setRandomNumber(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
		}
	});
#endif
	sqli->executeQuery(&q);
}

void TeBkUmLpqRouter::queries(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	CommonUtils::fastStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		LibpqQuery q;
		q.withParamInt4(rid);
#ifdef HAVE_LIBPQ
		q.withSelectQuery(WORLD_ONE_QUERY).withContext(&wlst).withCb0([](void* ctx, PGresult* res) {
			std::vector<TeBkUmLpqWorld>* wlst = (std::vector<TeBkUmLpqWorld>*)ctx;
			int cols = PQnfields(res);
			for (int j = 0; j < cols; ++j) {
				if(j==0) wlst->emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
				else wlst->back().setRandomNumber(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
			}
		});
#endif
		sqli->executeQuery(&q);
	}
}

void TeBkUmLpqRouter::queriesMulti(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	CommonUtils::fastStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	UpdQrData updt;
	updt.wlist = &wlst;
	updt.status = true;

	std::stringstream ss;
	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		ss << "select id, randomnumber from world where id = " << rid << ";";
	}

	LibpqQuery qu;
	qu.withSelectQuery(ss.str()).withContext(&wlst).withCb5([](void* ctx, int rn, int cn, char * d, int l) {
		std::vector<TeBkUmLpqWorld>* wlst = (std::vector<TeBkUmLpqWorld>*)ctx;
		int tmp = 0;
		CommonUtils::fastStrToNum(d, l, tmp);
		if(cn==0) {
			wlst->emplace_back(tmp);
		} else {
			wlst->back().setRandomNumber(tmp);
		}
	});
	sqli->executeMultiQuery(&qu);
}

std::string& TeBkUmLpqRouter::getUpdQuery(int count) {
	std::unordered_map<int, std::string>::iterator it = _qC.find(count);
	if(it!=_qC.end()) {
		return it->second;
	}

	std::stringstream ss;
	ss << "update world as t set randomnumber = case id";

	int pc = 1;
	for (int c = 0; c < count; ++c) {
		ss << " when $";
		ss << pc++;
		ss << " then $";
		ss << pc++;
	}
	ss << " else randomnumber end where id in (";
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
	CommonUtils::fastStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	std::string& query = getUpdQuery(queryCount);

	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		wlst.emplace_back();
		TeBkUmLpqWorld& w = wlst.back();

		LibpqQuery q;
		q.withParamInt4(rid);
#ifdef HAVE_LIBPQ
		q.withSelectQuery(WORLD_ONE_QUERY).withContext(&w).withCb0([](void* ctx, PGresult* res) {
			TeBkUmLpqWorld* w = (TeBkUmLpqWorld*)ctx;
			int cols = PQnfields(res);
			for (int j = 0; j < cols; ++j) {
				if(j==0)w->setId(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
				else w->setRandomNumber(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
			}
		});
#endif
		sqli->executeQuery(&q);

		int newRandomNumber = CommonUtils::fastrand(g_seed) % 10000 + 1;
		if(w.getRandomNumber() == newRandomNumber) {
			newRandomNumber += 1;
			if(newRandomNumber>=10000) {
				newRandomNumber = 1;
			}
		}
		w.setRandomNumber(newRandomNumber);
	}

	LibpqQuery qu;
	qu.withUpdateQuery(query);
	std::sort(wlst.begin(), wlst.end());
	for(std::vector<TeBkUmLpqWorld>::iterator it=wlst.begin(); it != wlst.end(); ++it) {
		qu.withParamInt4((*it).getId());
		qu.withParamInt4((*it).getRandomNumber());
	}
	for(std::vector<TeBkUmLpqWorld>::iterator it=wlst.begin(); it != wlst.end(); ++it) {
		qu.withParamInt4((*it).getId());
	}

	sqli->begin();
	sqli->executeUpdateQuery(&qu);
	sqli->commit();
}

void TeBkUmLpqRouter::updatesMulti(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	CommonUtils::fastStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	std::stringstream ssq;
	UpdQrData updt;
	updt.wlist = &wlst;
	updt.status = true;
	updt.queryCount = queryCount;

	updt.ss << "begin;update world as t set randomnumber = case id ";
	//ss << "begin;";//this creates a deadlock issue (like, DETAIL:  Process 16 waits for ShareLock on transaction 995; blocked by process 19.)

	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		ssq << "select id, randomnumber from world where id = " << rid << ";";
	}

	LibpqQuery qu;
#ifdef HAVE_LIBPQ
	qu.withSelectQuery(ssq.str()).withContext(&updt).withCb5([](void* ctx, int rn, int cn, char * d, int l) {
		UpdQrData* updt = (UpdQrData*)ctx;
		int tmp = 0;
		CommonUtils::fastStrToNum(d, l, tmp);
		if(cn==0) {
			updt->wlist->emplace_back(tmp);
		} else {
			TeBkUmLpqWorld& w = updt->wlist->back();
			int newRandomNumber = CommonUtils::fastrand(g_seed) % 10000 + 1;
			if(tmp == newRandomNumber) {
				newRandomNumber += 1;
				if(newRandomNumber>=10000) {
					newRandomNumber = 1;
				}
			}
			w.setRandomNumber(newRandomNumber);
			updt->ss << "when ";
			updt->ss << w.getId();
			updt->ss << " then ";
			updt->ss << newRandomNumber;
		}
	}).withFinalCb([](void* ctx, bool status, std::vector<PGresult*>* results, const std::string& q, int counter) {
		UpdQrData* updt = (UpdQrData*)ctx;
		updt->status = status;
	});
#endif
	sqli->executeMultiQuery(&qu);

	if(!updt.status) {
		return;
	}

	updt.ss << "else randomnumber end where id in (";
	for (int c = 0; c < queryCount; ++c) {
		updt.ss << wlst.at(c).getId();
		if(c<queryCount-1) {
			updt.ss << ",";
		}
	}
	updt.ss << ");commit;";

	qu.reset();
	qu.withUpdateQuery(updt.ss.str());
	sqli->executeUpdateMultiQuery(&qu);
}

void TeBkUmLpqRouter::updateCache() {
	CacheInterface* cchi = CacheManager::getImpl();
	LibpqDataSourceImpl* sqli = getDb();

	std::vector<TeBkUmLpqWorld> wlst;

	LibpqQuery q;
	q.withSelectQuery(WORLD_ALL_QUERY, false).withContext(&wlst).withCb6([](void* ctx, int row, int col, char* value) {
		std::vector<TeBkUmLpqWorld>* wlst = (std::vector<TeBkUmLpqWorld>*)ctx;
		if(col==0) {
			wlst->emplace_back(ntohl(*((uint32_t *) value)));
		} else {
			wlst->back().setRandomNumber(ntohl(*((uint32_t *) value)));
		}
	});
	sqli->executeQuery(&q);

	for(std::vector<TeBkUmLpqWorld>::iterator it=wlst.begin(); it != wlst.end(); ++it) {
		char str[12];
		sprintf(str, "%d;%d", (*it).getId(), (*it).getRandomNumber());
		cchi->setRaw((*it).getId(), str);
	}
	CacheManager::cleanImpl(cchi);
	CacheManager::triggerAppInitCompletion();
}

void TeBkUmLpqRouter::cachedWorlds(const char* q, int ql, std::vector<TeBkUmLpqWorld>& wlst) {
	int queryCount = 0;
	CommonUtils::fastStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	wlst.reserve(queryCount);

	CacheInterface* cchi = CacheManager::getImpl();

	std::vector<unsigned long long> keys;
	for (int c = 0; c < queryCount; ++c) {
		keys.emplace_back(CommonUtils::fastrand(g_seed) % 10000 + 1);
	}
	std::vector<std::string> values;
	cchi->getValues(keys, values);
	for (int c = 0; c < queryCount; ++c) {
		std::string& v = values.at(c);
		size_t fn = v.find(";");
		int tmp = 0;
		CommonUtils::fastStrToNum(v.substr(0, fn).c_str(), fn, tmp);
		int tmp1 = 0;
		CommonUtils::fastStrToNum(v.substr(fn+1).c_str(), v.length()-fn-1, tmp1);
		wlst.emplace_back(tmp, tmp1);
	}
	CacheManager::cleanImpl(cchi);
}

void TeBkUmLpqRouter::handleTemplate(HttpRequest* req, HttpResponse* res, BaseSocket* sif) {
	LibpqDataSourceImpl* sqli = getDb();

	Context ctx;
	std::list<TeBkUmLpqFortune> flst;

	LibpqQuery q;
#ifdef HAVE_LIBPQ
	q.withSelectQuery(FORTUNE_ALL_QUERY).withContext(&flst).withCb0([](void* ctx, PGresult* res) {
		std::list<TeBkUmLpqFortune>* flst = (std::list<TeBkUmLpqFortune>*)ctx;
		int cols = PQnfields(res);
		int rows = PQntuples(res);
		for(int i=0; i<rows; i++) {
			for (int j = 0; j < cols; ++j) {
				if(j==0) {
					flst->emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, i, j))));
				} else {
					TeBkUmLpqFortune& w = flst->back();
					w.message = CryptoHandler::sanitizeHtmlFast((const uint8_t *)PQgetvalue(res, i, j), (size_t)PQgetlength(res, i, j), w.message_i, w.allocd);
				}
			}
		}
	});
#endif
	sqli->executeQuery(&q);

	flst.emplace_back(0, "Additional fortune added at request time.");
	flst.sort();

	ctx.emplace("fortunes", &flst);

	fcpstream str;
	tmplFunc(&ctx, str);
	std::string out = str.str();
	std::string h;
	res->httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_TEXT_HTML, req->getHttpVers(), req->isClose(), out.length());
	sif->writeDirect(h, out);
}

//Do not use this class with non-embedded servers as it needs access to the underlying socket
//and writes the response directly to the socket, use TeBkUmLpqRouterPicoV for all lang-server implementations
bool TeBkUmLpqRouter::route(HttpRequest* req, HttpResponse* res, BaseSocket* sif) {
	std::string h;
	if(StringUtil::endsWith(req->getPath(), "/plaint")) {
		res->httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_TEXT_PLAIN, (int)HELLO_WORLD.length());
		sif->writeDirect(h, HELLO_WORLD);
	} else if(StringUtil::endsWith(req->getPath(), "/j")) {
		TeBkUmLpqMessage msg;
		msg.setMessage(HELLO_WORLD);
		JSONSerialize::serializeObject(&msg, m_ser, res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		sif->writeDirect(h, res->getContent());
	} else if(StringUtil::endsWith(req->getPath(), "/d")) {
		TeBkUmLpqWorld msg;
		db(msg);
		JSONSerialize::serializeObject(&msg, w_ser, res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->getHttpVers(), req->isClose());
		sif->writeDirect(h, res->getContent());
	} else if(StringUtil::endsWith(req->getPath(), "/que_")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		queries(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->getHttpVers(), req->isClose());
		sif->writeDirect(h, res->getContent());
	} else if(StringUtil::endsWith(req->getPath(), "/quer")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		queriesMulti(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->getHttpVers(), req->isClose());
		sif->writeDirect(h, res->getContent());
	} else if(StringUtil::endsWith(req->getPath(), "/fortu")) {
		handleTemplate(req, res, sif);
	} else if(StringUtil::endsWith(req->getPath(), "/upd_")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		updates(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->getHttpVers(), req->isClose());
		sif->writeDirect(h, res->getContent());
	} else if(StringUtil::endsWith(req->getPath(), "/updt")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		updatesMulti(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->getHttpVers(), req->isClose());
		sif->writeDirect(h, res->getContent());
	} else if(StringUtil::endsWith(req->getPath(), "/cached-wld")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		cachedWorlds(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->getHttpVers(), req->isClose());
		sif->writeDirect(h, res->getContent());
	} else {
		res->httpStatus(HTTPResponseStatus::NotFound).generateHeadResponse(h, req->getHttpVers(), true);
		sif->writeDirect(h);
	}
	return false;
}

TemplatePtr TeBkUmLpqRouter::tmplFunc;
Ser TeBkUmLpqRouter::m_ser;
Ser TeBkUmLpqRouter::w_ser;
SerCont TeBkUmLpqRouter::wcont_ser;

TeBkUmLpqRouter::TeBkUmLpqRouter() {
	sqli = NULL;
	tmplFunc = TemplateUtil::getTemplateFunc("t3", "tpe/fortunes.tpe");
	m_ser = Serializer::getSerFuncForObject("t3", "TeBkUmLpqMessage");
	w_ser = Serializer::getSerFuncForObject("t3", "TeBkUmLpqWorld");
	wcont_ser = Serializer::getSerFuncForObjectCont("t3", "TeBkUmLpqWorld", "std::vector");
}

TeBkUmLpqRouter::~TeBkUmLpqRouter() {
}

LibpqDataSourceImpl* TeBkUmLpqRouter::getDb() {
	if(sqli==NULL) {
		sqli = static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "t3"));
	}
	return sqli;
}

TeBkUmLpqRouterPicoV::TeBkUmLpqRouterPicoV() {
}

TeBkUmLpqRouterPicoV::~TeBkUmLpqRouterPicoV() {
}

void TeBkUmLpqRouterPicoV::handleTemplate(HttpResponse* res) {
	LibpqDataSourceImpl* sqli = getDb();

	Context ctx;
	std::list<TeBkUmLpqFortune> flst;

	LibpqQuery q;
#ifdef HAVE_LIBPQ
	q.withSelectQuery(FORTUNE_ALL_QUERY).withContext(&flst).withCb0([](void* ctx, PGresult* res) {
		std::list<TeBkUmLpqFortune>* flst = (std::list<TeBkUmLpqFortune>*)ctx;
		int cols = PQnfields(res);
		int rows = PQntuples(res);
		for(int i=0; i<rows; i++) {
			for (int j = 0; j < cols; ++j) {
				if(j==0) {
					flst->emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, i, j))));
				} else {
					TeBkUmLpqFortune& w = flst->back();
					w.message = CryptoHandler::sanitizeHtmlFast((const uint8_t *)PQgetvalue(res, i, j), (size_t)PQgetlength(res, i, j), w.message_i, w.allocd);
				}
			}
		}
	});
#endif
	sqli->executeQuery(&q);

	flst.emplace_back(0, "Additional fortune added at request time.");
	flst.sort();

	ctx.emplace("fortunes", &flst);

	fcpstream str;
	tmplFunc(&ctx, str);
	res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_TEXT_HTML).setContent(str.str());
}

bool TeBkUmLpqRouterPicoV::route(HttpRequest *req, HttpResponse *res, BaseSocket *sif) {
	if(StringUtil::endsWith(req->getPath(), "/plaint")) {
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_TEXT_PLAIN).setContent(HELLO_WORLD);
	} else if(StringUtil::endsWith(req->getPath(), "/j")) {
		TeBkUmLpqMessage msg;
		msg.setMessage(HELLO_WORLD);
		JSONSerialize::serializeObject(&msg, m_ser, res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/d")) {
		TeBkUmLpqWorld msg;
		db(msg);
		JSONSerialize::serializeObject(&msg, w_ser, res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/que_")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		queries(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/quer")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		queriesMulti(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/fortu")) {
		handleTemplate(res);
	} else if(StringUtil::endsWith(req->getPath(), "/upd_")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		updates(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/updt")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		updatesMulti(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/cached-wld")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqWorld> msg;
		cachedWorlds(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else {
		res->httpStatus(HTTPResponseStatus::NotFound);
	}
	res->setDone(true);
	return true;
}
