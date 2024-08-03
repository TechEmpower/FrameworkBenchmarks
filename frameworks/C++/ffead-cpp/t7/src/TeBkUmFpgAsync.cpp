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
 * TeBkUmFpgAsync.cpp
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */

#include "TeBkUmFpgAsync.h"

//This is template based json serialization approach
TemplateJson TeBkUmFpgAsyncWorld::tJ = TemplateJson("{\"id\":,\"randomNumber\":}", {6,22});
TemplateJson TeBkUmFpgAsyncWorld::tJc = TemplateJson("{\"id\":,\"randomNumber\":},", {6,22});

int TeBkUmFpgAsyncWorld::getId() const {
	return id;
}

void TeBkUmFpgAsyncWorld::setId(int id) {
	this->id = id;
}

int TeBkUmFpgAsyncWorld::getRandomNumber() const {
	return randomNumber;
}

void TeBkUmFpgAsyncWorld::setRandomNumber(int randomNumber) {
	this->randomNumber = randomNumber;
}

TeBkUmFpgAsyncWorld::TeBkUmFpgAsyncWorld(int id, int randomNumber) {
	this->id = id;
	this->randomNumber = randomNumber;
}

TeBkUmFpgAsyncWorld::TeBkUmFpgAsyncWorld(int id) {
	this->id = id;
	randomNumber = 0;
}

TeBkUmFpgAsyncWorld::TeBkUmFpgAsyncWorld() {
	id = 0;
	randomNumber = 0;
}

TeBkUmFpgAsyncWorld::~TeBkUmFpgAsyncWorld() {
}

int TeBkUmFpgAsyncFortune::getId() const {
	return id;
}

void TeBkUmFpgAsyncFortune::setId(int id) {
	this->id = id;
}

TeBkUmFpgAsyncFortune::TeBkUmFpgAsyncFortune(int id) {
	this->id = id;
	allocd = false;
}

TeBkUmFpgAsyncFortune::TeBkUmFpgAsyncFortune(int id, std::string message) {
	this->id = id;
	this->message_i = message;
	this->message = std::string_view(this->message_i);
	allocd = false;
}

TeBkUmFpgAsyncFortune::TeBkUmFpgAsyncFortune(int id, const uint8_t * buf, size_t len) {
	this->id = id;
	this->message = CryptoHandler::sanitizeHtmlFast(buf, len, this->message_i, this->allocd);
}

TeBkUmFpgAsyncFortune::~TeBkUmFpgAsyncFortune() {
	if(allocd && message.size()>0) {
		free((void *)message.data());
	}
}

bool TeBkUmFpgAsyncFortune::operator < (const TeBkUmFpgAsyncFortune& other) const {
	return message.compare(other.message)<0;
}

TemplateJson TeBkUmFpgAsyncMessage::tJ = TemplateJson("{\"message\":\"\"}", {12});

TeBkUmFpgAsyncMessage::TeBkUmFpgAsyncMessage() {
}

TeBkUmFpgAsyncMessage::TeBkUmFpgAsyncMessage(std::string message) {
	this->message = message;
}

TeBkUmFpgAsyncMessage::~TeBkUmFpgAsyncMessage() {
}

const std::string& TeBkUmFpgAsyncMessage::getMessage() const {
	return message;
}

void TeBkUmFpgAsyncMessage::setMessage(const std::string& message) {
	this->message = message;
}

const std::string TeBkUmFpgAsyncRouter::HELLO_WORLD = "Hello, World!";
const std::string TeBkUmFpgAsyncRouter::WORLD = "world";
const std::string TeBkUmFpgAsyncRouter::WORLD_ONE_QUERY = "select id,randomnumber from world where id=$1";
const std::string TeBkUmFpgAsyncRouter::WORLD_ALL_QUERY = "select id,randomnumber from world";
const std::string TeBkUmFpgAsyncRouter::FORTUNE_ALL_QUERY = "select id,message from fortune";
std::unordered_map<int, std::string> TeBkUmFpgAsyncRouter::_qC;
std::unordered_map<int, std::string> TeBkUmFpgAsyncRouter::_mqC;
int TeBkUmFpgAsyncRouter::g_seed = 0;

void TeBkUmFpgAsyncRouter::dbAsync(Writer* sif, HttpResponse* res) {
	LibpqDataSourceImpl* sqli = getDb(5);
	int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* q = areq->getQuery();
	q->withParamInt4(rid);
#ifdef HAVE_LIBPQ
	q->withSelectQuery(WORLD_ONE_QUERY).withContext(res, sif).withCb7([](void** ctx, int row, FpgIter* iter) {
		HttpResponse* res = (HttpResponse*)ctx[0];
		Writer* sif = (Writer*)ctx[1];
		TeBkUmFpgAsyncWorld w(ntohl(*((uint32_t *) iter->next().data())), ntohl(*((uint32_t *) iter->next().data())));
		w.tmplJson(res->getContentP());
		res->sendJson(sif);
		sif->unUse();
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmFpgAsyncRouter::queriesAsync(const char* q, int ql, Writer* sif, HttpResponse* res) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	std::vector<TeBkUmFpgAsyncWorld>* vec = new std::vector<TeBkUmFpgAsyncWorld>;
	vec->reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb(3);
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		LibpqQuery* q = areq->getQuery();
		q->withParamInt4(rid);
		q->withSelectQuery(WORLD_ONE_QUERY).withContext(vec).withCb7([](void** ctx, int row, FpgIter* iter) {
			std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[0];
			vec->emplace_back(ntohl(*((uint32_t *) iter->next().data())), ntohl(*((uint32_t *) iter->next().data())));
		});
	}
#ifdef HAVE_LIBPQ
	areq->withContext(res, sif, vec).withFinalCb1([](void** ctx, bool status, const std::string& q, int counter) {
		HttpResponse* res = (HttpResponse*)ctx[0];
		Writer* sif = (Writer*)ctx[1];
		std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[2];
		TeBkUmFpgAsyncWorld::tmplJson(*vec, res->getContentP());
		res->sendJson(sif);
		sif->unUse();
		delete vec;
	});
#endif
	sqli->postAsync(areq);
}

std::string& TeBkUmFpgAsyncRouter::getMultiQuery(int count) {
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

void TeBkUmFpgAsyncRouter::queriesMultiAsync(const char* q, int ql, Writer* sif, HttpResponse* res) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	std::vector<TeBkUmFpgAsyncWorld>* vec = new std::vector<TeBkUmFpgAsyncWorld>;
	vec->reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb(3);
	std::string& query = getMultiQuery(queryCount);

	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* qu = areq->getQuery();
	qu->withSelectQuery(query).withMulti(queryCount).withContext(vec).withCb7([](void** ctx, int row, FpgIter* iter) {
		std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[0];
		std::string_view id_ = iter->next();
		std::string_view rn_ = iter->next();
		int id = 0, rn = 0;
		CommonUtils::naiveStrToNum(id_.data(), id_.size(), id);
		CommonUtils::naiveStrToNum(rn_.data(), rn_.size(), rn);
		vec->emplace_back(id, rn);
	});
#ifdef HAVE_LIBPQ
	areq->withContext(res, sif, vec).withFinalCb1([](void** ctx, bool status, const std::string& q, int counter) {
		HttpResponse* res = (HttpResponse*)ctx[0];
		Writer* sif = (Writer*)ctx[1];
		std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[2];
		TeBkUmFpgAsyncWorld::tmplJson(*vec, res->getContentP());
		res->sendJson(sif);
		sif->unUse();
		delete vec;
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmFpgAsyncRouter::updatesMulti(const char* q, int ql, Writer* sif, HttpResponse* res) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	std::vector<TeBkUmFpgAsyncWorld>* vec = new std::vector<TeBkUmFpgAsyncWorld>;
	vec->reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb(3);
	std::string& query = getMultiQuery(queryCount);

	//req->ss << "begin;";//NEVER USE - this creates a deadlock issue (like, DETAIL:  Process 16 waits for ShareLock on transaction 995; blocked by process 19.)
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* qu = areq->getQuery();
	std::stringstream* ss = new std::stringstream;
	qu->withSelectQuery(query).withMulti(queryCount).withContext(vec, ss).withCb7([](void** ctx, int row, FpgIter* iter) {
		std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[0];
		std::stringstream* ss = (std::stringstream*)ctx[1];
		int id = 0;
		std::string_view id_ = iter->next();
		CommonUtils::naiveStrToNum(id_.data(), id_.size(), id);
		iter->next();
		int rd = CommonUtils::fastrand(g_seed) % 10000 + 1;
		vec->emplace_back(id, rd);
		*ss << "begin;update world set randomnumber = " << rd << " where id = " << id << ";commit;";
	});
#ifdef HAVE_LIBPQ
	areq->withContext(res, sif, vec, ss, sqli).withFinalCb1([](void** ctx, bool status, const std::string& q, int counter) {
		if(status) {
			std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[2];
			std::stringstream* ss = (std::stringstream*)ctx[3];
			LibpqDataSourceImpl* sqli = (LibpqDataSourceImpl*)ctx[4];

			LibpqAsyncReq* areq = sqli->getAsyncRequest();
			LibpqQuery* qu = areq->getQuery();
			qu->withUpdateQuery(ss->str()).withMulti((int)vec->size()*3);
			delete ss;

			areq->withContext(ctx[0], ctx[1], vec).withFinalCb1([](void** ctx, bool status, const std::string& q, int counter) {
				HttpResponse* res = (HttpResponse*)ctx[0];
				Writer* sif = (Writer*)ctx[1];
				std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[2];
				if(status) {
					TeBkUmFpgAsyncWorld::tmplJson(*vec, res->getContentP());
					res->sendJson(sif);
				} else {
					res->sendStatus(HTTPResponseStatus::InternalServerError, sif);
				}
				sif->unUse();
				delete vec;
			});
			sqli->postAsync(areq);
		}
	});
#endif
	sqli->postAsync(areq);
}

std::string& TeBkUmFpgAsyncRouter::getUpdQuery(int count) {
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
void TeBkUmFpgAsyncRouter::updatesAsyncb(const char* q, int ql, Writer* sif, HttpResponse* res) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	std::vector<TeBkUmFpgAsyncWorld>* vec = new std::vector<TeBkUmFpgAsyncWorld>;
	vec->reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb(3);
	LibpqAsyncReq* areq = sqli->getAsyncRequest();

	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		LibpqQuery* q = areq->getQuery();
		q->withParamInt4(rid);
		q->withSelectQuery(WORLD_ONE_QUERY).withContext(vec).withCb7([](void** ctx, int row, FpgIter* iter) {
			std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[0];
			LibpqQuery* q1 = (LibpqQuery*)ctx[1];
			int id = ntohl(*((uint32_t *) iter->next().data()));
			iter->next();
			int rd = CommonUtils::fastrand(g_seed) % 10000 + 1;
			vec->emplace_back(id, rd);
		});
	}
#ifdef HAVE_LIBPQ
	//void** ctx_ = (void**)malloc(sizeof(void *) * 6);
	//ctx_[0] = areq1;ctx_[1] = q1;ctx_[2] = res;ctx_[3] = sif;ctx_[4] = vec;ctx_[5] = sqli;
	areq->withContext(res, sif, vec, sqli).withFinalCb1([](void** ctx, bool status, const std::string& query, int counter) {
		//void** ctx_ = (void**)ctx[0];
		std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[2];
		LibpqDataSourceImpl* sqli = (LibpqDataSourceImpl*)ctx[3];
		
		LibpqAsyncReq* areq1 = sqli->getAsyncRequest();
		sqli->beginAsync(areq1);
		LibpqQuery* q1 = areq1->getQuery();
		q1->withUpdateQuery(getUpdQuery(vec->size()));
		for(auto w: *vec) {
			q1->withParamInt4(w.getId());
			q1->withParamInt4(w.getRandomNumber());
		}
		for(auto w: *vec) {
			q1->withParamInt4(w.getId());
		}
		sqli->commitAsync(areq1);

		areq1->withContext(ctx[0], ctx[1], vec).withFinalCb1([](void** ctx, bool status, const std::string& query, int counter) {
			HttpResponse* res = (HttpResponse*)ctx[0];
			Writer* sif = (Writer*)ctx[1];
			std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[2];
			if(status) {
				TeBkUmFpgAsyncWorld::tmplJson(*vec, res->getContentP());
				res->sendJson(sif);
			} else {
				res->sendStatus(HTTPResponseStatus::InternalServerError, sif);
			}
			sif->unUse();
			delete vec;
		});
		sqli->postAsync(areq1);
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmFpgAsyncRouter::updatesAsync(const char* q, int ql, Writer* sif, HttpResponse* res) {
	int queryCount = 0;
	CommonUtils::naiveStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	std::vector<TeBkUmFpgAsyncWorld>* vec = new std::vector<TeBkUmFpgAsyncWorld>;
	vec->reserve(queryCount);

	LibpqDataSourceImpl* sqli = getDb(3);
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	std::vector<std::string>* sv = new std::vector<std::string>;

	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		LibpqQuery* qu = areq->getQuery();
		qu->withParamInt4(rid);
		qu->withSelectQuery(WORLD_ONE_QUERY).withContext(vec, sv).withCb7([](void** ctx, int row, FpgIter* iter) {
			std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[0];
			std::vector<std::string>* sv = (std::vector<std::string>*)ctx[1];
			int id = ntohl(*((uint32_t *) iter->next().data()));
			iter->next();
			int rd = CommonUtils::fastrand(g_seed) % 10000 + 1;
			vec->emplace_back(id, rd);
			std::stringstream ss;
			ss << "update world set randomnumber = " << rd << " where id = " << id;
			sv->emplace_back(ss.str());
		});
	}
#ifdef HAVE_LIBPQ
	areq->withContext(res, sif, vec, sv, sqli).withFinalCb1([](void** ctx, bool status, const std::string& query, int counter) {
		std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[2];
		std::vector<std::string>* sv = (std::vector<std::string>*)ctx[3];
		LibpqDataSourceImpl* sqli = (LibpqDataSourceImpl*)ctx[4];

		LibpqAsyncReq* areq = sqli->getAsyncRequest();
		for (int i = 0; i < (int)sv->size(); ++i) {
			sqli->beginAsync(areq);
			LibpqQuery* q = areq->getQuery();
			q->withUpdateQuery(sv->at(i), false);
			sqli->commitAsync(areq);
		}
		delete sv;

		areq->withContext(ctx[0], ctx[1], vec).withFinalCb1([](void** ctx, bool status, const std::string& query, int counter) {
			HttpResponse* res = (HttpResponse*)ctx[0];
			Writer* sif = (Writer*)ctx[1];
			std::vector<TeBkUmFpgAsyncWorld>* vec = (std::vector<TeBkUmFpgAsyncWorld>*)ctx[2];
			if(status) {
				TeBkUmFpgAsyncWorld::tmplJson(*vec, res->getContentP());
				res->sendJson(sif);
			} else {
				res->sendStatus(HTTPResponseStatus::InternalServerError, sif);
			}
			sif->unUse();
			delete vec;
		});
		sqli->postAsync(areq);
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmFpgAsyncRouter::updateCache() {
	LibpqDataSourceImpl* sqli = getDb(1);

	AsyncCacheReq1* req = new AsyncCacheReq1;
	req->cchi = CacheManager::getImpl();

	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* q = areq->getQuery();
	q->withSelectQuery(WORLD_ALL_QUERY).withContext(req).withCb6([](void** ctx, int row, int col, char* value) {
		AsyncCacheReq1* req = (AsyncCacheReq1*)ctx[0];
		if(col==0) {
			req->vec.emplace_back(ntohl(*((uint32_t *) value)));
		} else {
			req->vec.back().setRandomNumber(ntohl(*((uint32_t *) value)));
		}
	});
	areq->withContext(req).withFinalCb1([](void** ctx, bool status, const std::string& query, int counter) {
		AsyncCacheReq1* req = (AsyncCacheReq1*)ctx[0];
		CacheInterface* cchi = req->cchi;
		try {
			for(std::vector<TeBkUmFpgAsyncWorld>::iterator it=req->vec.begin(); it != req->vec.end(); ++it) {
				char str[12];
				sprintf(str, "%d;%d", (*it).getId(), (*it).getRandomNumber());
				cchi->setRaw((*it).getId(), str);
			}
			CacheManager::cleanImpl(cchi);
			delete req;
			CacheManager::triggerAppInitCompletion("t7");
		} catch(const std::exception& e) {
			CacheManager::cleanImpl(cchi);
			delete req;
		}
	});
	sqli->postAsync(areq);
}
void TeBkUmFpgAsyncRouter::cachedWorlds(const char* q, int ql, std::vector<TeBkUmFpgAsyncWorld>& wlst) {
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


void TeBkUmFpgAsyncRouter::fortunes(Writer* sif, HttpResponse* res) {
	LibpqDataSourceImpl* sqli = getDb(7);
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* q = areq->getQuery();
	std::vector<TeBkUmFpgAsyncFortune*>* flst = new std::vector<TeBkUmFpgAsyncFortune*>;
#ifdef HAVE_LIBPQ
	q->withSelectQuery(FORTUNE_ALL_QUERY).withContext(flst).withCb7([](void** ctx, int row, FpgIter* iter) {
		std::vector<TeBkUmFpgAsyncFortune*>* flst = (std::vector<TeBkUmFpgAsyncFortune*>*)ctx[0];
		std::string_view i_ = iter->next();
		std::string_view m_ = iter->next();
		flst->push_back(new TeBkUmFpgAsyncFortune(ntohl(*((uint32_t *) i_.data())), (const uint8_t *)m_.data(), (size_t)m_.size()));
	});
	areq->withContext(res, sif, flst).withFinalCb1([](void** ctx, bool status, const std::string& query, int counter) {
		HttpResponse* res = (HttpResponse*)ctx[0];
		Writer* sif = (Writer*)ctx[1];
		std::vector<TeBkUmFpgAsyncFortune*>* flst = (std::vector<TeBkUmFpgAsyncFortune*>*)ctx[2];

		Context context;
		flst->push_back(new TeBkUmFpgAsyncFortune(0, "Additional fortune added at request time."));
		std::sort(flst->begin(), flst->end(), [](const TeBkUmFpgAsyncFortune* a, const TeBkUmFpgAsyncFortune* b) {
			return a->message < b->message;
		});
		context.emplace("fortunes", flst);

		tmplFunc(&context, res->getContent());
		res->sendHtml(sif);
		sif->unUse();
		for(int k=0;k<(int)flst->size();k++) {
	        delete flst->at(k);
	    }
		delete flst;
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmFpgAsyncRouter::routeAsync(HttpRequest* req, HttpResponse* res, Writer* sif) {
	AsyncReqData* dbreq = (AsyncReqData*)sif->getData();
	dbreq->reset();
	route(req, &dbreq->r, sif);
}

bool TeBkUmFpgAsyncRouter::route(HttpRequest* req, HttpResponse* res, Writer* sif) {
	sif->use();
	if(StringUtil::endsWith(req->getPath(), "/plaint")) {
		res->setContent(HELLO_WORLD).sendText(sif);
		sif->unUse();
	} else if(StringUtil::endsWith(req->getPath(), "/j")) {
		TeBkUmFpgAsyncMessage msg(HELLO_WORLD);
		msg.tmplJson(res->getContentP());
		res->sendJson(sif);
		sif->unUse();
	} else if(StringUtil::endsWith(req->getPath(), "/d")) {
		dbAsync(sif, res);
	} else if(StringUtil::endsWith(req->getPath(), "/quer")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		queriesAsync(params[0].val, params[0].val_len, sif, res);
	} else if(StringUtil::endsWith(req->getPath(), "/quem")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		queriesMultiAsync(params[0].val, params[0].val_len, sif, res);
	} else if(StringUtil::endsWith(req->getPath(), "/updt")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		updatesAsyncb(params[0].val, params[0].val_len, sif, res);
	} else if(StringUtil::endsWith(req->getPath(), "/upd_")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		updatesAsync(params[0].val, params[0].val_len, sif, res);
	} else if(StringUtil::endsWith(req->getPath(), "/updm")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		updatesMulti(params[0].val, params[0].val_len, sif, res);
	} else if(StringUtil::endsWith(req->getPath(), "/fortu")) {
		fortunes(sif, res);
	} else if(StringUtil::endsWith(req->getPath(), "/cached-wld")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmFpgAsyncWorld> vec;
		cachedWorlds(params[0].val, params[0].val_len, vec);
		TeBkUmFpgAsyncWorld::tmplJson(vec, res->getContentP());
		res->sendJson(sif);
		sif->unUse();
	} else {
		res->sendStatus(HTTPResponseStatus::NotFound, sif);
		sif->unUse();
	}
	return false;
}

TemplatePtr TeBkUmFpgAsyncRouter::tmplFunc;
Ser TeBkUmFpgAsyncRouter::m_ser;
Ser TeBkUmFpgAsyncRouter::w_ser;
SerCont TeBkUmFpgAsyncRouter::wcont_ser;

TeBkUmFpgAsyncRouter::TeBkUmFpgAsyncRouter() {
	sqli = NULL;
	tmplFunc = TemplateUtil::getTemplateFunc("t7", "tpe/fortunes.tpe");
	m_ser = Serializer::getSerFuncForObject("t7", "TeBkUmFpgAsyncMessage");
	w_ser = Serializer::getSerFuncForObject("t7", "TeBkUmFpgAsyncWorld");
	wcont_ser = Serializer::getSerFuncForObjectCont("t7", "TeBkUmFpgAsyncWorld", "std::vector");
}

TeBkUmFpgAsyncRouter::~TeBkUmFpgAsyncRouter() {
	if(sqli!=NULL) {
		DataSourceManager::cleanRawImpl(sqli);
	}
}

LibpqDataSourceImpl* TeBkUmFpgAsyncRouter::getDb(int max) {
	if(sqli==NULL) {
		sqli = static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "t7"));
	}
	return sqli;
}

LibpqDataSourceImpl* TeBkUmFpgAsyncRouterPooled::getDb(int max) {
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
			pool.push_back(static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "t7", true)));
		}
		inited = true;
	}
	return pool.at(pc%max);
}

TeBkUmFpgAsyncRouterPooled::TeBkUmFpgAsyncRouterPooled() {
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

TeBkUmFpgAsyncRouterPooled::~TeBkUmFpgAsyncRouterPooled() {
	for(auto sqli: pool) {
		if(sqli!=NULL) {
			DataSourceManager::cleanRawImpl(sqli);
		}
	}
}
