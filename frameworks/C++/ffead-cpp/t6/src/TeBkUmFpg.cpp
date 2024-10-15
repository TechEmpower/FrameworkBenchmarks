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
 * TeBkUmFpg.cpp
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */

#include "TeBkUmFpg.h"

//This is template based json serialization approach
TemplateJson TeBkUmFpgWorld::tJ = TemplateJson("{\"id\":,\"randomNumber\":}", {6,22});
TemplateJson TeBkUmFpgWorld::tJc = TemplateJson("{\"id\":,\"randomNumber\":},", {6,22});

int TeBkUmFpgWorld::getId() const {
	return id;
}

void TeBkUmFpgWorld::setId(int id) {
	this->id = id;
}

void TeBkUmFpgWorld::set(int id, int randomNumber) {
	this->id = id;
	this->randomNumber = randomNumber;
}

int TeBkUmFpgWorld::getRandomNumber() const {
	return randomNumber;
}

void TeBkUmFpgWorld::setRandomNumber(int randomNumber) {
	this->randomNumber = randomNumber;
}

TeBkUmFpgWorld::TeBkUmFpgWorld(int id) {
	this->id = id;
	randomNumber = 0;
}

TeBkUmFpgWorld::TeBkUmFpgWorld(int id, int randomNumber) {
	this->id = id;
	this->randomNumber = randomNumber;
}

bool TeBkUmFpgWorld::operator < (const TeBkUmFpgWorld& other) const {
	return id < other.id;
}

TeBkUmFpgWorld::TeBkUmFpgWorld() {
	id = 0;
	randomNumber = 0;
}

TeBkUmFpgWorld::~TeBkUmFpgWorld() {
}

int TeBkUmFpgFortune::getId() const {
	return id;
}

void TeBkUmFpgFortune::setId(int id) {
	this->id = id;
}

TeBkUmFpgFortune::TeBkUmFpgFortune(int id) {
	this->id = id;
	allocd = false;
}

TeBkUmFpgFortune::TeBkUmFpgFortune(int id, std::string message) {
	this->id = id;
	this->message_i = message;
	this->message = std::string_view(this->message_i);
	allocd = false;
}

TeBkUmFpgFortune::TeBkUmFpgFortune(int id, const uint8_t * buf, size_t len) {
	this->id = id;
	this->message = CryptoHandler::sanitizeHtmlFast(buf, len, this->message_i, this->allocd);
}

TeBkUmFpgFortune::~TeBkUmFpgFortune() {
	if(allocd && message.size()>0) {
		free((void *)message.data());
	}
}

bool TeBkUmFpgFortune::operator < (const TeBkUmFpgFortune& other) const {
	return message.compare(other.message)<0;
}

TemplateJson TeBkUmFpgMessage::tJ = TemplateJson("{\"message\":\"\"}", {12});

TeBkUmFpgMessage::TeBkUmFpgMessage() {
}

TeBkUmFpgMessage::TeBkUmFpgMessage(std::string message) {
	this->message = message;
}

TeBkUmFpgMessage::~TeBkUmFpgMessage() {
}

const std::string& TeBkUmFpgMessage::getMessage() const {
	return message;
}

void TeBkUmFpgMessage::setMessage(const std::string& message) {
	this->message = message;
}

const std::string TeBkUmFpgRouter::HELLO_WORLD = "Hello, World!";
const std::string TeBkUmFpgRouter::WORLD = "world";
const std::string TeBkUmFpgRouter::WORLD_ONE_QUERY = "select id,randomnumber from world where id=$1";
const std::string TeBkUmFpgRouter::WORLD_ALL_QUERY = "select id,randomnumber from world";
const std::string TeBkUmFpgRouter::FORTUNE_ALL_QUERY = "select id,message from fortune";
int TeBkUmFpgRouter::g_seed = 0;

void TeBkUmFpgRouter::db(TeBkUmFpgWorld& w) {
	LibpqDataSourceImpl* sqli = getDb();
	int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
	LibpqQuery q;
	q.withParamInt4(rid);
#ifdef HAVE_LIBPQ
	q.withSelectQuery(WORLD_ONE_QUERY).withContext(&w).withCb4([](void** ctx, int rn, int cn, char* name, char* value, int vlen) {
		TeBkUmFpgWorld* w = (TeBkUmFpgWorld*)ctx[0];
		if(cn==0) w->setId(ntohl(*((uint32_t *) value)));
		else w->setRandomNumber(ntohl(*((uint32_t *) value)));
	});
#endif
	sqli->executeQuery(&q);
}

void TeBkUmFpgRouter::queries(const char* q, int ql, std::vector<TeBkUmFpgWorld>& wlst) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));
	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();
	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		LibpqQuery q;
		q.withParamInt4(rid);
#ifdef HAVE_LIBPQ
		q.withSelectQuery(WORLD_ONE_QUERY).withContext(&wlst).withCb4([](void** ctx, int rn, int cn, char* name, char* value, int vlen) {
			std::vector<TeBkUmFpgWorld>* wlst = (std::vector<TeBkUmFpgWorld>*)ctx[0];
			if(cn==0) wlst->emplace_back(ntohl(*((uint32_t *) value)));
			else wlst->back().setRandomNumber(ntohl(*((uint32_t *) value)));
		});
#endif
		sqli->executeQuery(&q);
	}
}

std::string& TeBkUmFpgRouter::getMultiQuery(int count) {
	std::unordered_map<int, std::string>::iterator it = _mqC.find(count);
	if(it!=_mqC.end()) {
		return it->second;
	}

	std::stringstream ss;
	for (int c = 0; c < count; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		ss << "select id, randomnumber from world where id = " << rid << ";";
	}

	_mqC[count] = std::move(ss.str());
	return _mqC[count];
}

void TeBkUmFpgRouter::queriesMulti(const char* q, int ql, std::vector<TeBkUmFpgWorld>& wlst) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));
	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();
	std::string& query = getMultiQuery(queryCount);
	LibpqQuery qu;
	qu.withSelectQuery(query).withMulti(queryCount).withContext(&wlst).withCb5([](void** ctx, int rn, int cn, char * d, int l) {
		std::vector<TeBkUmFpgWorld>* wlst = (std::vector<TeBkUmFpgWorld>*)ctx[0];
		int tmp = 0;
		CommonUtils::naiveStrToNum(d, l, tmp);
		if(cn==0) wlst->emplace_back(tmp);
		else wlst->back().setRandomNumber(tmp);
	});
	sqli->executeMultiQuery(&qu);
}

std::string& TeBkUmFpgRouter::getUpdQuery(int count) {
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

void TeBkUmFpgRouter::updates(const char* q, int ql, std::vector<TeBkUmFpgWorld>& wlst) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();
	std::string& query = getUpdQuery(queryCount);

	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		LibpqQuery q;
		q.withParamInt4(rid);
#ifdef HAVE_LIBPQ
		q.withSelectQuery(WORLD_ONE_QUERY).withContext(&wlst).withCb4([](void** ctx, int rn, int cn, char* name, char* value, int vlen) {
			std::vector<TeBkUmFpgWorld>* wlst = (std::vector<TeBkUmFpgWorld>*)ctx[0];
			if(cn==0) wlst->emplace_back(ntohl(*((uint32_t *) value)));
			else wlst->back().setRandomNumber(CommonUtils::fastrand(g_seed) % 10000 + 1);
		});
#endif
		sqli->executeQuery(&q);
	}

	LibpqQuery qu;
	qu.withUpdateQuery(query);
	std::sort(wlst.begin(), wlst.end());
	for(std::vector<TeBkUmFpgWorld>::iterator it=wlst.begin(); it != wlst.end(); ++it) {
		qu.withParamInt4((*it).getId());
		qu.withParamInt4((*it).getRandomNumber());
	}
	for(std::vector<TeBkUmFpgWorld>::iterator it=wlst.begin(); it != wlst.end(); ++it) {
		qu.withParamInt4((*it).getId());
	}

	sqli->begin();
	sqli->executeUpdateQuery(&qu);
	sqli->commit();
}

void TeBkUmFpgRouter::updatesMulti(const char* q, int ql, std::vector<TeBkUmFpgWorld>& wlst) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	wlst.reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb();

	std::stringstream ssq;
	t6::UpdQrData updt;
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
	qu.withSelectQuery(ssq.str()).withMulti(queryCount).withContext(&updt).withCb5([](void** ctx, int rn, int cn, char * d, int l) {
		t6::UpdQrData* updt = (t6::UpdQrData*)ctx[0];
		int tmp = 0;
		CommonUtils::naiveStrToNum(d, l, tmp);
		if(cn==0) updt->wlist->emplace_back(tmp);
		else {
			TeBkUmFpgWorld& w = updt->wlist->back();
			int newRandomNumber = CommonUtils::fastrand(g_seed) % 10000 + 1;
			if(tmp == newRandomNumber) {
				newRandomNumber += 1;
				if(newRandomNumber>=10000) {
					newRandomNumber = 1;
				}
			}
			w.setRandomNumber(newRandomNumber);
			updt->ss << " when ";
			updt->ss << w.getId();
			updt->ss << " then ";
			updt->ss << newRandomNumber;
		}
	}).withFinalCb1([](void** ctx, bool status, const std::string& q, int counter) {
		t6::UpdQrData* updt = (t6::UpdQrData*)ctx[0];
		updt->status = status;
	});
#endif
	sqli->executeMultiQuery(&qu);

	if(!updt.status) {
		return;
	}

	updt.ss << " else randomnumber end where id in (";
	for (int c = 0; c < queryCount; ++c) {
		updt.ss << wlst.at(c).getId();
		if(c<queryCount-1) {
			updt.ss << ",";
		}
	}
	updt.ss << ");commit;";

	qu.reset();
	qu.withUpdateQuery(updt.ss.str()).withMulti(3);//begin-query-commit
	sqli->executeUpdateMultiQuery(&qu);
}

void TeBkUmFpgRouter::updateCache() {
	CacheInterface* cchi = CacheManager::getImpl();
	LibpqDataSourceImpl* sqli = getDb();
	std::vector<TeBkUmFpgWorld> wlst;

	LibpqQuery q;
	q.withSelectQuery(WORLD_ALL_QUERY, false).withContext(&wlst).withCb5([](void** ctx, int row, int cn, char* value, int vlen) {
		std::vector<TeBkUmFpgWorld>* wlst = (std::vector<TeBkUmFpgWorld>*)ctx[0];
		int tmp = 0;
		CommonUtils::naiveStrToNum(value, vlen, tmp);
		if(cn==0) wlst->emplace_back(tmp);
		else wlst->back().setRandomNumber(tmp);
		if(row%500==0 || row==9999) {
			fprintf(stderr, "rows = %d\n", row);
		}
	});
	sqli->executeQuery(&q);

	for(std::vector<TeBkUmFpgWorld>::iterator it=wlst.begin(); it != wlst.end(); ++it) {
		char str[12];
		sprintf(str, "%d;%d", (*it).getId(), (*it).getRandomNumber());
		cchi->setRaw((*it).getId(), str);
	}
	CacheManager::cleanImpl(cchi);
	//Thread::sSleep(30);
	CacheManager::triggerAppInitCompletion();
}

void TeBkUmFpgRouter::cachedWorlds(const char* q, int ql, std::vector<TeBkUmFpgWorld>& wlst) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
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
		CommonUtils::naiveStrToNum(v.substr(0, fn).c_str(), fn, tmp);
		int tmp1 = 0;
		CommonUtils::naiveStrToNum(v.substr(fn+1).c_str(), v.length()-fn-1, tmp1);
		wlst.emplace_back(tmp, tmp1);
	}
	CacheManager::cleanImpl(cchi);
}

void TeBkUmFpgRouter::handleTemplate(HttpRequest* req, HttpResponse* res, Writer* sif) {
	LibpqDataSourceImpl* sqli = getDb();
	Context ctx;
	std::vector<TeBkUmFpgFortune*> flst;

	LibpqQuery q;
#ifdef HAVE_LIBPQ
	q.withSelectQuery(FORTUNE_ALL_QUERY).withContext(&flst).withCb7([](void** ctx, int row, FpgIter* iter) {
		std::vector<TeBkUmFpgFortune*>* flst = (std::vector<TeBkUmFpgFortune*>*)ctx[0];
		std::string_view id_ = iter->next();
		int id = ntohl(*((uint32_t *) id_.data()));
		std::string_view ms = iter->next();
		flst->push_back(new TeBkUmFpgFortune(id, (const uint8_t *)ms.data(), (size_t)ms.length()));
	});
#endif
	sqli->executeQuery(&q);

	flst.push_back(new TeBkUmFpgFortune(0, "Additional fortune added at request time."));
	std::sort(flst.begin(), flst.end(), [](const TeBkUmFpgFortune* a, const TeBkUmFpgFortune* b) {
		return a->message < b->message;
	});

	ctx.emplace("fortunes", &flst);

	tmplFunc(&ctx, res->getContent());
	res->sendHtml(sif);
	for(int k=0;k<(int)flst.size();k++) {
        delete flst.at(k);
    }
}

//Do not use this class with non-embedded servers as it needs access to the underlying socket
//and writes the response directly to the socket, use TeBkUmFpgRouterPicoV for all lang-server implementations
bool TeBkUmFpgRouter::route(HttpRequest* req, HttpResponse* res, Writer* sif) {
	if(StringUtil::endsWith(req->getPath(), "/plaint")) {
		res->setContent(HELLO_WORLD).sendText(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/j")) {
		TeBkUmFpgMessage msg(HELLO_WORLD);
		msg.tmplJson(res->getContentP());
		res->sendJson(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/d")) {
		TeBkUmFpgWorld msg;
		db(msg);
		msg.tmplJson(res->getContentP());
		res->sendJson(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/quer")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		queries(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->sendJson(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/quem")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		queriesMulti(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->sendJson(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/fortu")) {
		handleTemplate(req, res, sif);
	} else if(StringUtil::endsWith(req->getPath(), "/updt")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		updates(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->sendJson(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/updm")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		updatesMulti(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->sendJson(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/cached-wld")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		cachedWorlds(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->sendJson(sif);
	} else {
		res->sendStatus(HTTPResponseStatus::NotFound, sif);
	}
	return false;
}

TemplatePtr TeBkUmFpgRouter::tmplFunc;
Ser TeBkUmFpgRouter::m_ser;
Ser TeBkUmFpgRouter::w_ser;
SerCont TeBkUmFpgRouter::wcont_ser;

TeBkUmFpgRouter::TeBkUmFpgRouter() {
	sqli = NULL;
	tmplFunc = TemplateUtil::getTemplateFunc("t6", "tpe/fortunes.tpe");
	m_ser = Serializer::getSerFuncForObject("t6", "TeBkUmFpgMessage");
	w_ser = Serializer::getSerFuncForObject("t6", "TeBkUmFpgWorld");
	wcont_ser = Serializer::getSerFuncForObjectCont("t6", "TeBkUmFpgWorld", "std::vector");
}

TeBkUmFpgRouter::~TeBkUmFpgRouter() {
}

LibpqDataSourceImpl* TeBkUmFpgRouter::getDb() {
	if(sqli==NULL) {
		sqli = static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "t6"));
	}
	return sqli;
}

TeBkUmFpgRouterPicoV::TeBkUmFpgRouterPicoV() {
}

TeBkUmFpgRouterPicoV::~TeBkUmFpgRouterPicoV() {
}

void TeBkUmFpgRouterPicoV::handleTemplate(HttpResponse* res) {
	LibpqDataSourceImpl* sqli = getDb();

	Context ctx;
	std::vector<TeBkUmFpgFortune*> flst;

	LibpqQuery q;
#ifdef HAVE_LIBPQ
	q.withSelectQuery(FORTUNE_ALL_QUERY).withContext(&flst).withCb7([](void** ctx, int row, FpgIter* iter) {
		std::vector<TeBkUmFpgFortune*>* flst = (std::vector<TeBkUmFpgFortune*>*)ctx[0];
		std::string_view id_ = iter->next();
		int id = ntohl(*((uint32_t *) id_.data()));
		std::string_view ms = iter->next();
		flst->push_back(new TeBkUmFpgFortune(id, (const uint8_t *)ms.data(), (size_t)ms.length()));
	});
#endif
	sqli->executeQuery(&q);

	flst.push_back(new TeBkUmFpgFortune(0, "Additional fortune added at request time."));
	std::sort(flst.begin(), flst.end(), [](const TeBkUmFpgFortune* a, const TeBkUmFpgFortune* b) {
		return a->message < b->message;
	});

	ctx.emplace("fortunes", &flst);

	tmplFunc(&ctx, res->getContent());
	for(int k=0;k<(int)flst.size();k++) {
        delete flst.at(k);
    }
	res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_TEXT_HTML);
}

bool TeBkUmFpgRouterPicoV::route(HttpRequest *req, HttpResponse *res, Writer *sif) {
	if(StringUtil::endsWith(req->getPath(), "/plaint")) {
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_TEXT_PLAIN).setContent(HELLO_WORLD);
	} else if(StringUtil::endsWith(req->getPath(), "/j")) {
		TeBkUmFpgMessage msg(HELLO_WORLD);
		msg.tmplJson(res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/d")) {
		TeBkUmFpgWorld msg;
		db(msg);
		msg.tmplJson(res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/quer")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		queries(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/quem")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		queriesMulti(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/fortu")) {
		handleTemplate(res);
	} else if(StringUtil::endsWith(req->getPath(), "/updt")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		updates(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/updm")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		updatesMulti(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else if(StringUtil::endsWith(req->getPath(), "/cached-wld")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgWorld> msg;
		cachedWorlds(params[0].val, params[0].val_len, msg);
		TeBkUmFpgWorld::tmplJson(msg, res->getContentP());
		res->httpStatus(HTTPResponseStatus::Ok).setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
	} else {
		res->httpStatus(HTTPResponseStatus::NotFound);
	}
	res->setDone(true);
	return true;
}
