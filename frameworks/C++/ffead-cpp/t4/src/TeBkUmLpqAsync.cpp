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
 * TeBkUmLpqAsync.cpp
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */

#include "TeBkUmLpqAsync.h"

//This is template based json serialization approach
TemplateJson TeBkUmLpqAsyncWorld::tJ = TemplateJson("{\"id\":,\"randomNumber\":}", {6,22});
TemplateJson TeBkUmLpqAsyncWorld::tJc = TemplateJson("{\"id\":,\"randomNumber\":},", {6,22});

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

TeBkUmLpqAsyncFortune::TeBkUmLpqAsyncFortune(int id, const uint8_t * buf, size_t len) {
	this->id = id;
	this->message = CryptoHandler::sanitizeHtmlFast(buf, len, this->message_i, this->allocd);
}

TeBkUmLpqAsyncFortune::~TeBkUmLpqAsyncFortune() {
	if(allocd && message.size()>0) {
		free((void *)message.data());
	}
}

bool TeBkUmLpqAsyncFortune::operator < (const TeBkUmLpqAsyncFortune& other) const {
	return message.compare(other.message)<0;
}

TemplateJson TeBkUmLpqAsyncMessage::tJ = TemplateJson("{\"message\":\"\"}", {12});

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
const std::string TeBkUmLpqAsyncRouter::WORLD = "world";
const std::string TeBkUmLpqAsyncRouter::WORLD_ONE_QUERY = "select id,randomnumber from world where id=$1";
const std::string TeBkUmLpqAsyncRouter::WORLD_ALL_QUERY = "select id,randomnumber from world";
const std::string TeBkUmLpqAsyncRouter::FORTUNE_ALL_QUERY = "select id,message from fortune";
std::unordered_map<int, std::string> TeBkUmLpqAsyncRouter::_qC;
std::unordered_map<int, std::string> TeBkUmLpqAsyncRouter::_mqC;
int TeBkUmLpqAsyncRouter::g_seed = 0;

void TeBkUmLpqAsyncRouter::dbAsync(Writer* sif) {
	LibpqDataSourceImpl* sqli = getDb(5);
	int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* q = areq->getQuery();
	q->withParamInt4(rid);
#ifdef HAVE_LIBPQ
	q->withSelectQuery(WORLD_ONE_QUERY).withContext(sif).withCb0([](void** ctx, PGresult* res) {
		Writer* sif = (Writer*)ctx[0];
		AsyncReqData* reqdt = (AsyncReqData*)sif->getData();
		TeBkUmLpqAsyncWorld w(ntohl(*((uint32_t *) PQgetvalue(res, 0, 0))), ntohl(*((uint32_t *) PQgetvalue(res, 0, 1))));
		w.tmplJson(reqdt->r.getContentP());
		reqdt->r.sendJson(sif);
		sif->unUse();
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmLpqAsyncRouter::queriesAsync(const char* q, int ql, Writer* sif) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	LibpqDataSourceImpl* sqli = getDb(3);
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		LibpqQuery* q = areq->getQuery();
		q->withParamInt4(rid);
		q->withSelectQuery(WORLD_ONE_QUERY);
	}
#ifdef HAVE_LIBPQ
	areq->withContext(sif).withFinalCb([](void** ctx, bool status, std::vector<PGresult*>* results, const std::string& q, int counter) {
		Writer* sif = (Writer*)ctx[0];
		std::vector<TeBkUmLpqAsyncWorld> vec;
		vec.reserve((int)results->size());
		for (int i = 0; i < (int)results->size(); ++i) {
			PGresult* res = results->at(i);
			vec.emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, 0, 0))), ntohl(*((uint32_t *) PQgetvalue(res, 0, 1))));
		}

		AsyncReqData* reqdt = (AsyncReqData*)sif->getData();
		TeBkUmLpqAsyncWorld::tmplJson(vec, reqdt->r.getContentP());
		reqdt->r.sendJson(sif);
		sif->unUse();
	});
#endif
	sqli->postAsync(areq);
}

std::string& TeBkUmLpqAsyncRouter::getMultiQuery(int count) {
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

void TeBkUmLpqAsyncRouter::queriesMultiAsync(const char* q, int ql, Writer* sif) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	LibpqDataSourceImpl* sqli = getDb(3);

	std::string& query = getMultiQuery(queryCount);

	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* qu = areq->getQuery();
	qu->withSelectQuery(query).withMulti(queryCount);
#ifdef HAVE_LIBPQ
	areq->withContext(sif).withFinalCb([](void** ctx,bool status, std::vector<PGresult*>* results, const std::string& q, int counter) {
		Writer* sif = (Writer*)ctx[0];
		std::vector<TeBkUmLpqAsyncWorld> vec;
		vec.reserve((int)results->size());
		for (int i = 0; i < (int)results->size(); ++i) {
			PGresult* res = results->at(i);
			int id = 0, rd = 0;
			CommonUtils::naiveStrToNum(PQgetvalue(res, 0, 0), PQgetlength(res, 0, 0), id);
			CommonUtils::naiveStrToNum(PQgetvalue(res, 0, 1), PQgetlength(res, 0, 1), rd);
			vec.emplace_back(id, rd);
		}

		AsyncReqData* reqdt = (AsyncReqData*)sif->getData();
		TeBkUmLpqAsyncWorld::tmplJson(vec, reqdt->r.getContentP());
		reqdt->r.sendJson(sif);
		sif->unUse();
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmLpqAsyncRouter::updatesMulti(const char* q, int ql, AsyncUpdatesReq* req) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	req->vec.reserve(queryCount);
	req->sqli = getDb(3);

	std::string& query = getMultiQuery(queryCount);

	//req->ss << "begin;";//NEVER USE - this creates a deadlock issue (like, DETAIL:  Process 16 waits for ShareLock on transaction 995; blocked by process 19.)
	LibpqAsyncReq* areq = req->sqli->getAsyncRequest();
	LibpqQuery* qu = areq->getQuery();
	qu->withSelectQuery(query).withMulti(queryCount);
#ifdef HAVE_LIBPQ
	areq->withContext(req).withFinalCb([](void** ctx,bool status, std::vector<PGresult*>* results, const std::string& q, int counter) {
		AsyncUpdatesReq* req = (AsyncUpdatesReq*)ctx[0];
		if(status) {
			int queryCount = (int)results->size();

			std::stringstream ss;
			for (int i = 0; i < queryCount; ++i) {
				PGresult* res = results->at(i);
				int id = 0, rd = CommonUtils::fastrand(g_seed) % 10000 + 1;
				CommonUtils::naiveStrToNum(PQgetvalue(res, 0, 0), PQgetlength(res, 0, 0), id);
				req->vec.emplace_back(id, rd);
				ss << "begin;update world set randomnumber = " << rd << " where id = " << id << ";commit;";
				/*int cols = PQnfields(res);
				for (int j = 0; j < cols; ++j) {
					int tmp = 0;
					CommonUtils::naiveStrToNum(PQgetvalue(res, 0, j), PQgetlength(res, 0, j), tmp);
					if(j==0) req->vec.emplace_back(tmp);
					else {
						TeBkUmLpqAsyncWorld& w = req->vec.back();
						int newRandomNumber = CommonUtils::fastrand(g_seed) % 10000 + 1;
						if(tmp == newRandomNumber) {
							newRandomNumber += 1;
							if(newRandomNumber>=10000) {
								newRandomNumber = 1;
							}
						}
						w.setRandomNumber(newRandomNumber);
						ss << "begin;update world set randomnumber = " << newRandomNumber << " where id = " << w.getId() << ";commit;";
					}
				}*/
			}

			LibpqAsyncReq* areq = req->sqli->getAsyncRequest();
			LibpqQuery* qu = areq->getQuery();
			qu->withUpdateQuery(ss.str()).withMulti(queryCount*3);

			areq->withContext(req).withFinalCb([](void** ctx,bool status, std::vector<PGresult*>* results, const std::string& q, int counter) {
				AsyncUpdatesReq* req = (AsyncUpdatesReq*)ctx[0];
				if(status) {
					AsyncReqData* reqdt = (AsyncReqData*)req->sif->getData();
					TeBkUmLpqAsyncWorld::tmplJson(req->vec, reqdt->r.getContentP());
					reqdt->r.sendJson(req->sif);
				} else {
					HttpResponse r;
					r.sendStatus(HTTPResponseStatus::InternalServerError, req->sif);
				}
				req->sif->unUse();
				delete req;
			});
			req->sqli->postAsync(areq);
		}
	});
#endif
	req->sqli->postAsync(areq);
}

std::string& TeBkUmLpqAsyncRouter::getUpdQuery(int count) {
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
void TeBkUmLpqAsyncRouter::updatesAsyncb(const char* q, int ql, AsyncUpdatesReq* req) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	req->vec.reserve(queryCount);
	req->sqli = getDb(3);

	LibpqAsyncReq* areq = req->sqli->getAsyncRequest();
	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		LibpqQuery* q = areq->getQuery();
		q->withParamInt4(rid);
		q->withSelectQuery(WORLD_ONE_QUERY);
	}
#ifdef HAVE_LIBPQ
	areq->withContext(req).withFinalCb([](void** ctx,bool status, std::vector<PGresult*>* results, const std::string& query, int counter) {
		AsyncUpdatesReq* req = (AsyncUpdatesReq*)ctx[0];

		int queryCount = (int)results->size();

		LibpqAsyncReq* areq = req->sqli->getAsyncRequest();
		req->sqli->beginAsync(areq);
		LibpqQuery* q = areq->getQuery();
		q->withUpdateQuery(getUpdQuery(queryCount)).withContext(req);

		for (int i = 0; i < queryCount; ++i) {
			PGresult* res = results->at(i);
			int id = ntohl(*((uint32_t *) PQgetvalue(res, 0, 0)));
			int rd = CommonUtils::fastrand(g_seed) % 10000 + 1;
			req->vec.emplace_back(id, rd);
			q->withParamInt4(id);
			q->withParamInt4(rd);
			/*int cols = PQnfields(res);
			for (int j = 0; j < cols; ++j) {
				if(j==0) req->vec.emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
				else {
					int tmp = ntohl(*((uint32_t *) PQgetvalue(res, 0, j)));
					TeBkUmLpqAsyncWorld& w = req->vec.back();
					int newRandomNumber = CommonUtils::fastrand(g_seed) % 10000 + 1;
					if(tmp == newRandomNumber) {
						newRandomNumber += 1;
						if(newRandomNumber>=10000) {
							newRandomNumber = 1;
						}
					}
					w.setRandomNumber(newRandomNumber);
					q->withParamInt4(w.getId());
					q->withParamInt4(w.getRandomNumber());
				}
			}*/
		}
		for(auto w: req->vec) {
			q->withParamInt4(w.getId());
		}
		req->sqli->commitAsync(areq);

		areq->withContext(req).withFinalCb([](void** ctx,bool status, std::vector<PGresult*>* results, const std::string& query, int counter) {
			AsyncUpdatesReq* req = (AsyncUpdatesReq*)ctx[0];
			if(status) {
				AsyncReqData* reqdt = (AsyncReqData*)req->sif->getData();
				TeBkUmLpqAsyncWorld::tmplJson(req->vec, reqdt->r.getContentP());
				reqdt->r.sendJson(req->sif);
			} else {
				HttpResponse r;
				r.sendStatus(HTTPResponseStatus::InternalServerError, req->sif);
			}
			req->sif->unUse();
			delete req;
		});
		req->sqli->postAsync(areq);
	});
#endif
	req->sqli->postAsync(areq);
}

void TeBkUmLpqAsyncRouter::updatesAsync(const char* q, int ql, AsyncUpdatesReq* req) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	req->vec.reserve(queryCount);

	req->sqli = getDb(3);

	LibpqAsyncReq* areq = req->sqli->getAsyncRequest();
	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		LibpqQuery* qu = areq->getQuery();
		qu->withParamInt4(rid);
		qu->withSelectQuery(WORLD_ONE_QUERY);
	}
#ifdef HAVE_LIBPQ
	areq->withContext(req).withFinalCb([](void** ctx,bool status, std::vector<PGresult*>* results, const std::string& query, int counter) {
		AsyncUpdatesReq* req = (AsyncUpdatesReq*)ctx[0];
		LibpqAsyncReq* areq = req->sqli->getAsyncRequest();

		for (int i = 0; i < (int)results->size(); ++i) {
			PGresult* res = results->at(i);
			int id = ntohl(*((uint32_t *) PQgetvalue(res, 0, 0)));
			int rd = CommonUtils::fastrand(g_seed) % 10000 + 1;
			req->vec.emplace_back(id, rd);

			std::stringstream ss;
			ss << "update world set randomnumber = " << rd << " where id = " << id;

			req->sqli->beginAsync(areq);
			LibpqQuery* q = areq->getQuery();
			q->withUpdateQuery(ss.str(), false);
			req->sqli->commitAsync(areq);

			/*int cols = PQnfields(res);
			for (int j = 0; j < cols; ++j) {
				if(j==0) req->vec.emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
				else {
					int tmp = ntohl(*((uint32_t *) PQgetvalue(res, 0, j)));
					TeBkUmLpqAsyncWorld& w = req->vec.back();
					int newRandomNumber = CommonUtils::fastrand(g_seed) % 10000 + 1;
					if(tmp == newRandomNumber) {
						newRandomNumber += 1;
						if(newRandomNumber>=10000) {
							newRandomNumber = 1;
						}
					}
					w.setRandomNumber(newRandomNumber);

					std::stringstream ss;
					ss << "update world set randomnumber = " << newRandomNumber << " where id = " << w.getId();

					req->sqli->beginAsync(areq);
					LibpqQuery* q = areq->getQuery();
					q->withUpdateQuery(ss.str(), false);
					req->sqli->commitAsync(areq);
				}
			}*/
		}

		areq->withContext(req).withFinalCb([](void** ctx,bool status, std::vector<PGresult*>* results, const std::string& query, int counter) {
			AsyncUpdatesReq* req = (AsyncUpdatesReq*)ctx[0];
			AsyncReqData* reqdt = (AsyncReqData*)req->sif->getData();
			if(status) {
				TeBkUmLpqAsyncWorld::tmplJson(req->vec, reqdt->r.getContentP());
				reqdt->r.sendJson(req->sif);
			} else {
				reqdt->r.sendStatus(HTTPResponseStatus::InternalServerError, req->sif);
			}
			req->sif->unUse();
			delete req;
		});
		req->sqli->postAsync(areq);
	});
#endif
	req->sqli->postAsync(areq);
}

void TeBkUmLpqAsyncRouter::updateCache() {
	LibpqDataSourceImpl* sqli = getDb(1);

	AsyncCacheReq* req = new AsyncCacheReq;
	req->cchi = CacheManager::getImpl();

	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* q = areq->getQuery();
	q->withSelectQuery(WORLD_ALL_QUERY).withContext(req).withCb3([](void** ctx,bool endofdata, int row, int col, char* value) {
		AsyncCacheReq* req = (AsyncCacheReq*)ctx[0];
		if(col==0) {
			req->vec.emplace_back(ntohl(*((uint32_t *) value)));
		} else {
			req->vec.back().setRandomNumber(ntohl(*((uint32_t *) value)));
		}

		if(endofdata) {
			CacheInterface* cchi = req->cchi;
			try {
				for(std::vector<TeBkUmLpqAsyncWorld>::iterator it=req->vec.begin(); it != req->vec.end(); ++it) {
					char str[12];
					sprintf(str, "%d;%d", (*it).getId(), (*it).getRandomNumber());
					cchi->setRaw((*it).getId(), str);
				}
				CacheManager::cleanImpl(cchi);
				delete req;
				CacheManager::triggerAppInitCompletion("t4");
			} catch(const std::exception& e) {
				CacheManager::cleanImpl(cchi);
				delete req;
			}
		}
	});
	sqli->postAsync(areq);
}
void TeBkUmLpqAsyncRouter::cachedWorlds(const char* q, int ql, std::vector<TeBkUmLpqAsyncWorld>& wlst) {
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


void TeBkUmLpqAsyncRouter::fortunes(Writer* sif) {
	LibpqDataSourceImpl* sqli = getDb(7);
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* q = areq->getQuery();
#ifdef HAVE_LIBPQ
	q->withSelectQuery(FORTUNE_ALL_QUERY).withContext(sif).withCb0([](void** ctx,PGresult* res) {
		Writer* sif = (Writer*)ctx[0];
		std::vector<TeBkUmLpqAsyncFortune*> flst;
		int rows = PQntuples(res);
		flst.reserve((size_t)rows+1);
		for(int i=0; i<rows; i++) {
			flst.push_back(new TeBkUmLpqAsyncFortune(ntohl(*((uint32_t *) PQgetvalue(res, i, 0))), (const uint8_t *)PQgetvalue(res, i, 1), (size_t)PQgetlength(res, i, 1)));
		}

		Context context;
		flst.push_back(new TeBkUmLpqAsyncFortune(0, "Additional fortune added at request time."));
		std::sort(flst.begin(), flst.end(), [](const TeBkUmLpqAsyncFortune* a, const TeBkUmLpqAsyncFortune* b) {
			return a->message < b->message;
		});
		context.emplace("fortunes", &flst);

		AsyncReqData* dbreq = (AsyncReqData*)sif->getData();
		tmplFunc(&context, dbreq->r.getContent());
		dbreq->r.sendHtml(sif);
		sif->unUse();
		for(int k=0;k<(int)flst.size();k++) {
	        delete flst.at(k);
	    }
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmLpqAsyncRouter::routeAsync(HttpRequest* req, HttpResponse* res, Writer* sif) {
	AsyncReqData* dbreq = (AsyncReqData*)sif->getData();
	dbreq->reset();
	route(req, &dbreq->r, sif);
}

bool TeBkUmLpqAsyncRouter::route(HttpRequest* req, HttpResponse* res, Writer* sif) {
	sif->use();
	if(StringUtil::endsWith(req->getPath(), "/plaint")) {
		AsyncReqData* dbreq = (AsyncReqData*)sif->getData();
		res->setContent(HELLO_WORLD).sendText(sif);
		sif->unUse();
	} else if(StringUtil::endsWith(req->getPath(), "/j")) {
		TeBkUmLpqAsyncMessage msg(HELLO_WORLD);
		AsyncReqData* dbreq = (AsyncReqData*)sif->getData();
		msg.tmplJson(dbreq->r.getContentP());
		dbreq->r.sendJson(sif);
		sif->unUse();
	} else if(StringUtil::endsWith(req->getPath(), "/d")) {
		dbAsync(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/quer")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		queriesAsync(params[0].val, params[0].val_len, sif);
	} else if(StringUtil::endsWith(req->getPath(), "/quem")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		queriesMultiAsync(params[0].val, params[0].val_len, sif);
	} else if(StringUtil::endsWith(req->getPath(), "/updt")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncUpdatesReq* ar = new AsyncUpdatesReq;
		ar->sif = sif;
		updatesAsyncb(params[0].val, params[0].val_len, ar);
	} else if(StringUtil::endsWith(req->getPath(), "/upd_")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncUpdatesReq* ar = new AsyncUpdatesReq;
		ar->sif = sif;
		updatesAsync(params[0].val, params[0].val_len, ar);
	} else if(StringUtil::endsWith(req->getPath(), "/updm")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncUpdatesReq* ar = new AsyncUpdatesReq;
		ar->sif = sif;
		updatesMulti(params[0].val, params[0].val_len, ar);
	} else if(StringUtil::endsWith(req->getPath(), "/fortu")) {
		fortunes(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/cached-wld")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmLpqAsyncWorld> vec;
		cachedWorlds(params[0].val, params[0].val_len, vec);
		AsyncReqData* dbreq = (AsyncReqData*)sif->getData();
		TeBkUmLpqAsyncWorld::tmplJson(vec, dbreq->r.getContentP());
		dbreq->r.sendJson(sif);
		sif->unUse();
	} else {
		res->sendStatus(HTTPResponseStatus::NotFound, sif);
		sif->unUse();
	}
	return false;
}

TemplatePtr TeBkUmLpqAsyncRouter::tmplFunc;
Ser TeBkUmLpqAsyncRouter::m_ser;
Ser TeBkUmLpqAsyncRouter::w_ser;
SerCont TeBkUmLpqAsyncRouter::wcont_ser;

TeBkUmLpqAsyncRouter::TeBkUmLpqAsyncRouter() {
	Writer::registerWriterEventCallback([](Writer* bs, int type) {
		if(type==1) {
			bs->setData(new AsyncReqData, [](void* data) {
				delete (AsyncReqData*)data;
			});
		}
	});
	sqli = NULL;
	tmplFunc = TemplateUtil::getTemplateFunc("t4", "tpe/fortunes.tpe");
	m_ser = Serializer::getSerFuncForObject("t4", "TeBkUmLpqAsyncMessage");
	w_ser = Serializer::getSerFuncForObject("t4", "TeBkUmLpqAsyncWorld");
	wcont_ser = Serializer::getSerFuncForObjectCont("t4", "TeBkUmLpqAsyncWorld", "std::vector");
}

TeBkUmLpqAsyncRouter::~TeBkUmLpqAsyncRouter() {
	if(sqli!=NULL) {
		DataSourceManager::cleanRawImpl(sqli);
	}
}

LibpqDataSourceImpl* TeBkUmLpqAsyncRouter::getDb(int max) {
	if(sqli==NULL) {
		sqli = static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "t4"));
	}
	return sqli;
}

LibpqDataSourceImpl* TeBkUmLpqAsyncRouterPooled::getDb(int max) {
	if(max==0) {
		max = maxconns;
	} else {
		max = std::min(max, maxconns);
	}
	int pc = 0;
	if(inited) {
		pc = ++opt;
		if(pc>=INT_MAX-1) {
			opt = 0;
		}
	} else {
		for (int var = 0; var < maxconns; ++var) {
			pool.push_back(static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "t4", true)));
		}
		inited = true;
	}
	return pool.at(pc%max);
}

TeBkUmLpqAsyncRouterPooled::TeBkUmLpqAsyncRouterPooled() {
	Writer::registerWriterEventCallback([](Writer* bs, int type) {
		if(type==1) {
			bs->setData(new AsyncReqData, [](void* data) {
				delete (AsyncReqData*)data;
			});
		}
	});
	maxconns = 7;
	propMap props = ConfigurationData::getAppProperties();
	if(props.size()>0) {
		if(props.find("dbpoolsize")!=props.end()) {
			try {
				maxconns = CastUtil::toInt(props["dbpoolsize"]);
			} catch(...) {
			}
		}
	}
	inited = false;
	opt = 0;
}

TeBkUmLpqAsyncRouterPooled::~TeBkUmLpqAsyncRouterPooled() {
	for(auto sqli: pool) {
		if(sqli!=NULL) {
			DataSourceManager::cleanRawImpl(sqli);
		}
	}
}
