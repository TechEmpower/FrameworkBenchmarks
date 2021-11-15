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
 * TeBkUmLpqQwAsyncUm.cpp
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */

#include "TeBkUmLpqQwAsync.h"

int TeBkUmLpqQwAsyncWorld::getId() const {
	return id;
}

void TeBkUmLpqQwAsyncWorld::setId(int id) {
	this->id = id;
}

int TeBkUmLpqQwAsyncWorld::getRandomNumber() const {
	return randomNumber;
}

void TeBkUmLpqQwAsyncWorld::setRandomNumber(int randomNumber) {
	this->randomNumber = randomNumber;
}

TeBkUmLpqQwAsyncWorld::TeBkUmLpqQwAsyncWorld(int id, int randomNumber) {
	this->id = id;
	this->randomNumber = randomNumber;
}

TeBkUmLpqQwAsyncWorld::TeBkUmLpqQwAsyncWorld(int id) {
	this->id = id;
	randomNumber = 0;
}

TeBkUmLpqQwAsyncWorld::TeBkUmLpqQwAsyncWorld() {
	id = 0;
	randomNumber = 0;
}

TeBkUmLpqQwAsyncWorld::~TeBkUmLpqQwAsyncWorld() {
}

int TeBkUmLpqQwAsyncFortune::getId() const {
	return id;
}

void TeBkUmLpqQwAsyncFortune::setId(int id) {
	this->id = id;
}

TeBkUmLpqQwAsyncFortune::TeBkUmLpqQwAsyncFortune(int id) {
	this->id = id;
	allocd = false;
}

TeBkUmLpqQwAsyncFortune::TeBkUmLpqQwAsyncFortune(int id, std::string message) {
	this->id = id;
	this->message_i = message;
	this->message = std::string_view(this->message_i);
	allocd = false;
}

TeBkUmLpqQwAsyncFortune::TeBkUmLpqQwAsyncFortune() {
	id = 0;
	allocd = false;
}

TeBkUmLpqQwAsyncFortune::~TeBkUmLpqQwAsyncFortune() {
	if(allocd && message.size()>0) {
		free((void *)message.data());
	}
}

bool TeBkUmLpqQwAsyncFortune::operator < (const TeBkUmLpqQwAsyncFortune& other) const {
	return message.compare(other.message)<0;
}

TeBkUmLpqQwAsyncMessage::TeBkUmLpqQwAsyncMessage() {
}

TeBkUmLpqQwAsyncMessage::TeBkUmLpqQwAsyncMessage(std::string message) {
	this->message = message;
}

TeBkUmLpqQwAsyncMessage::~TeBkUmLpqQwAsyncMessage() {
}

const std::string& TeBkUmLpqQwAsyncMessage::getMessage() const {
	return message;
}

void TeBkUmLpqQwAsyncMessage::setMessage(const std::string& message) {
	this->message = message;
}

const std::string TeBkUmLpqQwAsyncRouter::HELLO_WORLD = "Hello, World!";
const std::string TeBkUmLpqQwAsyncRouter::WORLD = "world";
const std::string TeBkUmLpqQwAsyncRouter::WORLD_ONE_QUERY = "select id,randomnumber from world where id=$1";
const std::string TeBkUmLpqQwAsyncRouter::WORLD_ALL_QUERY = "select id,randomnumber from world";
const std::string TeBkUmLpqQwAsyncRouter::FORTUNE_ALL_QUERY = "select id,message from fortune";
std::unordered_map<int, std::string> TeBkUmLpqQwAsyncRouter::_qC;
int TeBkUmLpqQwAsyncRouter::g_seed = 0;

void TeBkUmLpqQwAsyncRouter::dbAsync(BaseSocket* sif) {
	LibpqDataSourceImpl* sqli = getDb(5);
	int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* q = areq->getQuery();
	q->withParamInt4(rid);
#ifdef HAVE_LIBPQ
	q->withSelectQuery(WORLD_ONE_QUERY, true).withContext(sif).withCb0([](void* ctx, PGresult* res) {
		BaseSocket* sif = (BaseSocket*)ctx;

		TeBkUmLpqQwAsyncWorld* wo = new TeBkUmLpqQwAsyncWorld;
		int cols = PQnfields(res);
		for (int j = 0; j < cols; ++j) {
			if(j==0)wo->setId(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
			else wo->setRandomNumber(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
		}

		sif->queueWrite([](BaseSocket* sif, void* arg) {
			TeBkUmLpqQwAsyncWorld* wo = (TeBkUmLpqQwAsyncWorld*)arg;

			HttpResponse r;
			std::string h;
	#ifdef HAVE_RAPID_JSON
			rapidjson::StringBuffer s;
			rapidjson::Writer<rapidjson::StringBuffer> w(s);
			wo->toJson(w);
			r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false, s.GetSize());
			sif->writeDirect(h, s.GetString(), s.GetSize());
	#else
			JSONSerialize::serializeObject(wo, w_ser, r.getContentP());
			r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
			sif->writeDirect(h, r.getContent());
	#endif
			sif->unUse();
			delete wo;
		}, wo);
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmLpqQwAsyncRouter::queriesAsync(const char* q, int ql, BaseSocket* sif) {
	int queryCount = 0;
	CommonUtils::fastStrToNum(q, ql, queryCount);
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
	areq->withFinalCb(sif, [](void* ctx, bool status, std::vector<PGresult*>* results, const std::string& q, int counter) {
		BaseSocket* sif = (BaseSocket*)ctx;
		std::vector<TeBkUmLpqQwAsyncWorld>* vec = new std::vector<TeBkUmLpqQwAsyncWorld>;
		vec->reserve(results->size());
		for (int i = 0; i < (int)results->size(); ++i) {
			PGresult* res = results->at(i);
			int cols = PQnfields(res);
			for (int j = 0; j < cols; ++j) {
				if(j==0) vec->emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
				else vec->back().setRandomNumber(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
			}
		}

		sif->queueWrite([](BaseSocket* sif, void* arg) {
			std::vector<TeBkUmLpqQwAsyncWorld>* vec = (std::vector<TeBkUmLpqQwAsyncWorld>*)arg;

			HttpResponse r;
			std::string h;
	#ifdef HAVE_RAPID_JSON
			rapidjson::StringBuffer s;
			rapidjson::Writer<rapidjson::StringBuffer> w(s);
			TeBkUmLpqQwAsyncWorld::toJson(*vec, w);
			r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false, s.GetSize());
			sif->writeDirect(h, s.GetString(), s.GetSize());
	#else
			JSONSerialize::serializeObjectCont(vec, wcont_ser, "vector", r.getContentP());
			r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false);
			sif->writeDirect(h, r.getContent());
	#endif
			sif->unUse();

			delete vec;
		}, vec);
	});
#endif
	sqli->postAsync(areq);
}

void TeBkUmLpqQwAsyncRouter::queriesMultiAsync(const char* q, int ql, BaseSocket* sif) {
	int queryCount = 0;
	CommonUtils::fastStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	LibpqDataSourceImpl* sqli = getDb(3);

	std::stringstream ss;
	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		ss << "select id, randomnumber from world where id = " << rid << ";";
	}

	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* qu = areq->getQuery();
	qu->withSelectQuery(ss.str()).withMulti();
#ifdef HAVE_LIBPQ
	areq->withFinalCb(sif, [](void* ctx, bool status, std::vector<PGresult*>* results, const std::string& q, int counter) {
		BaseSocket* sif = (BaseSocket*)ctx;
		std::vector<TeBkUmLpqQwAsyncWorld>* vec = new std::vector<TeBkUmLpqQwAsyncWorld>;
		vec->reserve(results->size());
		for (int i = 0; i < (int)results->size(); ++i) {
			PGresult* res = results->at(i);
			int cols = PQnfields(res);
			for (int j = 0; j < cols; ++j) {
				int tmp = 0;
				CommonUtils::fastStrToNum(PQgetvalue(res, 0, j), PQgetlength(res, 0, j), tmp);
				if(j==0) vec->emplace_back(tmp);
				else vec->back().setRandomNumber(tmp);
			}
		}

		sif->queueWrite([](BaseSocket* sif, void* arg) {
			std::vector<TeBkUmLpqQwAsyncWorld>* vec = (std::vector<TeBkUmLpqQwAsyncWorld>*)arg;

			HttpResponse r;
			std::string h;
	#ifdef HAVE_RAPID_JSON
			rapidjson::StringBuffer s;
			rapidjson::Writer<rapidjson::StringBuffer> w(s);
			TeBkUmLpqQwAsyncWorld::toJson(*vec, w);
			r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false, s.GetSize());
			sif->writeDirect(h, s.GetString(), s.GetSize());
	#else
			JSONSerialize::serializeObjectCont(vec, wcont_ser, "vector", r.getContentP());
			r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false);
			sif->writeDirect(h, r.getContent());
	#endif
			sif->unUse();

			delete vec;
		}, vec);
	});
#endif
	sqli->postAsync(areq, queryCount);
}

void TeBkUmLpqQwAsyncRouter::updatesMulti(const char* q, int ql, AsyncUpdatesReqWq* req) {
	int queryCount = 0;
	CommonUtils::fastStrToNum(q, ql, queryCount);
	queryCount = std::max(1, std::min(queryCount, 500));

	req->vec.reserve(queryCount);
	req->sqli = getDb(3);

	std::stringstream ss;
	for (int c = 0; c < queryCount; ++c) {
		int rid = CommonUtils::fastrand(g_seed) % 10000 + 1;
		ss << "select id, randomnumber from world where id = " << rid << ";";
	}

	//req->ss << "begin;";//NEVER USE - this creates a deadlock issue (like, DETAIL:  Process 16 waits for ShareLock on transaction 995; blocked by process 19.)
	LibpqAsyncReq* areq = req->sqli->getAsyncRequest();
	LibpqQuery* qu = areq->getQuery();
	qu->withSelectQuery(ss.str()).withMulti();
#ifdef HAVE_LIBPQ
	areq->withFinalCb(req, [](void* ctx, bool status, std::vector<PGresult*>* results, const std::string& q, int counter) {
		AsyncUpdatesReqWq* req = (AsyncUpdatesReqWq*)ctx;
		if(status) {
			int queryCount = (int)results->size();

			std::stringstream ss;
			for (int i = 0; i < queryCount; ++i) {
				PGresult* res = results->at(i);
				int cols = PQnfields(res);
				for (int j = 0; j < cols; ++j) {
					int tmp = 0;
					CommonUtils::fastStrToNum(PQgetvalue(res, 0, j), PQgetlength(res, 0, j), tmp);
					if(j==0) req->vec.emplace_back(tmp);
					else {
						TeBkUmLpqQwAsyncWorld& w = req->vec.back();
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
				}
			}

			LibpqAsyncReq* areq = req->sqli->getAsyncRequest();
			LibpqQuery* qu = areq->getQuery();
			qu->withUpdateQuery(ss.str()).withMulti();

			areq->withFinalCb(req, [](void* ctx, bool status, std::vector<PGresult*>* results, const std::string& q, int counter) {
				AsyncUpdatesReqWq* req = (AsyncUpdatesReqWq*)ctx;
				req->sif->queueWrite([](BaseSocket* sif, void* arg) {
					AsyncUpdatesReqWq* req = (AsyncUpdatesReqWq*)arg;
					if(req->vec.size()>0) {
						HttpResponse r;
						std::string h;
	#ifdef HAVE_RAPID_JSON
						rapidjson::StringBuffer s;
						rapidjson::Writer<rapidjson::StringBuffer> w(s);
						TeBkUmLpqQwAsyncWorld::toJson(req->vec, w);
						r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false, s.GetSize());
						req->sif->writeDirect(h, s.GetString(), s.GetSize());
	#else
						JSONSerialize::serializeObjectCont(&req->vec, wcont_ser, "vector", r.getContentP());
						r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false);
						req->sif->writeDirect(h, r.getContent());
	#endif
					} else {
						HttpResponse r;
						std::string h;
						r.httpStatus(HTTPResponseStatus::InternalServerError).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->httpVers, true);
						req->sif->writeDirect(h);
					}
					req->sif->unUse();
					delete req;
				}, ctx);
			});
			req->sqli->postAsync(areq, queryCount*3);
		}
	});
#endif
	req->sqli->postAsync(areq, queryCount);
}

std::string& TeBkUmLpqQwAsyncRouter::getUpdQuery(int count) {
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
void TeBkUmLpqQwAsyncRouter::updatesAsyncb(const char* q, int ql, AsyncUpdatesReqWq* req) {
	int queryCount = 0;
	CommonUtils::fastStrToNum(q, ql, queryCount);
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
	areq->withFinalCb(req, [](void* ctx, bool status, std::vector<PGresult*>* results, const std::string& query, int counter) {
		AsyncUpdatesReqWq* req = (AsyncUpdatesReqWq*)ctx;

		int queryCount = (int)results->size();

		LibpqAsyncReq* areq = req->sqli->getAsyncRequest();
		req->sqli->beginAsync(areq);
		LibpqQuery* q = areq->getQuery();
		q->withUpdateQuery(getUpdQuery(queryCount)).withContext(req);

		for (int i = 0; i < queryCount; ++i) {
			PGresult* res = results->at(i);
			int cols = PQnfields(res);
			for (int j = 0; j < cols; ++j) {
				if(j==0) req->vec.emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
				else {
					int tmp = ntohl(*((uint32_t *) PQgetvalue(res, 0, j)));
					TeBkUmLpqQwAsyncWorld& w = req->vec.back();
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
			}
		}
		for(auto w: req->vec) {
			q->withParamInt4(w.getId());
		}
		req->sqli->commitAsync(areq);

		areq->withFinalCb(req, [](void* ctx, bool status, std::vector<PGresult*>* results, const std::string& query, int counter) {
			AsyncUpdatesReqWq* req = (AsyncUpdatesReqWq*)ctx;
			req->sif->queueWrite([](BaseSocket* sif, void* arg) {
				AsyncUpdatesReqWq* req = (AsyncUpdatesReqWq*)arg;
				if(req->vec.size()>0) {
					HttpResponse r;
					std::string h;
	#ifdef HAVE_RAPID_JSON
					rapidjson::StringBuffer s;
					rapidjson::Writer<rapidjson::StringBuffer> w(s);
					TeBkUmLpqQwAsyncWorld::toJson(req->vec, w);
					r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false, s.GetSize());
					req->sif->writeDirect(h, s.GetString(), s.GetSize());
	#else
					JSONSerialize::serializeObjectCont(&req->vec, wcont_ser, "vector", r.getContentP());
					r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false);
					req->sif->writeDirect(h, r.getContent());
	#endif
				} else {
					HttpResponse r;
					std::string h;
					r.httpStatus(HTTPResponseStatus::InternalServerError).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->httpVers, true);
					req->sif->writeDirect(h);
				}
				req->sif->unUse();
				delete req;
			}, ctx);
		});
		req->sqli->postAsync(areq);
	});
#endif
	req->sqli->postAsync(areq);
}

void TeBkUmLpqQwAsyncRouter::updatesAsync(const char* q, int ql, AsyncUpdatesReqWq* req) {
	int queryCount = 0;
	CommonUtils::fastStrToNum(q, ql, queryCount);
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
	areq->withFinalCb(req, [](void* ctx, bool status, std::vector<PGresult*>* results, const std::string& query, int counter) {
		AsyncUpdatesReqWq* req = (AsyncUpdatesReqWq*)ctx;
		LibpqAsyncReq* areq = req->sqli->getAsyncRequest();

		for (int i = 0; i < (int)results->size(); ++i) {
			PGresult* res = results->at(i);
			int cols = PQnfields(res);
			for (int j = 0; j < cols; ++j) {
				if(j==0) req->vec.emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, 0, j))));
				else {
					int tmp = ntohl(*((uint32_t *) PQgetvalue(res, 0, j)));
					TeBkUmLpqQwAsyncWorld& w = req->vec.back();
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
			}
		}

		areq->withFinalCb(req, [](void* ctx, bool status, std::vector<PGresult*>* results, const std::string& query, int counter) {
			AsyncUpdatesReqWq* req = (AsyncUpdatesReqWq*)ctx;
			req->sif->queueWrite([](BaseSocket* sif, void* arg) {
				AsyncUpdatesReqWq* req = (AsyncUpdatesReqWq*)arg;
				if(req->vec.size()>0) {
					HttpResponse r;
					std::string h;
	#ifdef HAVE_RAPID_JSON
					rapidjson::StringBuffer s;
					rapidjson::Writer<rapidjson::StringBuffer> w(s);
					TeBkUmLpqQwAsyncWorld::toJson(req->vec, w);
					r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false, s.GetSize());
					req->sif->writeDirect(h, s.GetString(), s.GetSize());
	#else
					JSONSerialize::serializeObjectCont(&req->vec, wcont_ser, "vector", r.getContentP());
					r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, 1.1, false);
					req->sif->writeDirect(h, r.getContent());
	#endif
				} else {
					HttpResponse r;
					std::string h;
					r.httpStatus(HTTPResponseStatus::InternalServerError).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_APPLICATION_JSON, req->httpVers, true);
					req->sif->writeDirect(h);
				}
				req->sif->unUse();
				delete req;
			}, ctx);
		});
		req->sqli->postAsync(areq);
	});
#endif
	req->sqli->postAsync(areq);
}

void TeBkUmLpqQwAsyncRouter::fortunes(BaseSocket* sif) {
	LibpqDataSourceImpl* sqli = getDb(7);
	LibpqAsyncReq* areq = sqli->getAsyncRequest();
	LibpqQuery* q = areq->getQuery();
#ifdef HAVE_LIBPQ
	q->withSelectQuery(FORTUNE_ALL_QUERY).withContext(sif).withCb0([](void* ctx, PGresult* res) {
		BaseSocket* sif = (BaseSocket*)ctx;

		std::list<TeBkUmLpqQwAsyncFortune>* flst = new std::list<TeBkUmLpqQwAsyncFortune>;
		int cols = PQnfields(res);
		int rows = PQntuples(res);
		for(int i=0; i<rows; i++) {
			for (int j = 0; j < cols; ++j) {
				if(j==0) {
					flst->emplace_back(ntohl(*((uint32_t *) PQgetvalue(res, i, j))));
				} else {
					TeBkUmLpqQwAsyncFortune& w = flst->back();
					w.message = CryptoHandler::sanitizeHtmlFast((const uint8_t *)PQgetvalue(res, i, j), (size_t)PQgetlength(res, i, j), w.message_i, w.allocd);
				}
			}
		}

		sif->queueWrite([](BaseSocket* sif, void* arg) {
			std::list<TeBkUmLpqQwAsyncFortune>* flst = (std::list<TeBkUmLpqQwAsyncFortune>*)arg;

			Context context;

			flst->emplace_back(0, "Additional fortune added at request time.");
			flst->sort();

			context.emplace("fortunes", flst);

			fcpstream str;
			tmplFunc(&context, str);
			std::string out = str.str();
			HttpResponse r;
			std::string h;
			r.httpStatus(HTTPResponseStatus::Ok).generateHeadResponse(h, ContentTypes::CONTENT_TYPE_TEXT_HTML, 1.1, false, (int)out.length());
			sif->writeDirect(h, out);
			sif->unUse();

			delete flst;
		}, flst);
	});
#endif
	sqli->postAsync(areq);
}

bool TeBkUmLpqQwAsyncRouter::route(HttpRequest* req, HttpResponse* res, BaseSocket* sif) {
	sif->use();
	if(StringUtil::endsWith(req->getPath(), "/d")) {
		dbAsync(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/quer")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		queriesAsync(params[0].val, params[0].val_len, sif);
	} else if(StringUtil::endsWith(req->getPath(), "/quem")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		queriesMultiAsync(params[0].val, params[0].val_len, sif);
	} else if(StringUtil::endsWith(req->getPath(), "/updm")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncUpdatesReqWq* ar = new AsyncUpdatesReqWq;
		ar->sif = sif;
		ar->httpVers = req->getHttpVers();
		ar->conn_clos = req->isClose();
		updatesMulti(params[0].val, params[0].val_len, ar);
	} else if(StringUtil::endsWith(req->getPath(), "/fortu")) {
		fortunes(sif);
	} else if(StringUtil::endsWith(req->getPath(), "/updt") || StringUtil::endsWith(req->getPath(), "/bupdt")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncUpdatesReqWq* ar = new AsyncUpdatesReqWq;
		ar->sif = sif;
		ar->httpVers = req->getHttpVers();
		ar->conn_clos = req->isClose();
		updatesAsyncb(params[0].val, params[0].val_len, ar);
	} else if(StringUtil::endsWith(req->getPath(), "/upd_")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		AsyncUpdatesReqWq* ar = new AsyncUpdatesReqWq;
		ar->sif = sif;
		ar->httpVers = req->getHttpVers();
		ar->conn_clos = req->isClose();
		updatesAsync(params[0].val, params[0].val_len, ar);
	} else {
		std::string h;
		res->httpStatus(HTTPResponseStatus::NotFound).generateHeadResponse(h, req->getHttpVers(), true);
		sif->writeDirect(h);
		sif->unUse();
	}
	return false;
}

TemplatePtr TeBkUmLpqQwAsyncRouter::tmplFunc;
Ser TeBkUmLpqQwAsyncRouter::m_ser;
Ser TeBkUmLpqQwAsyncRouter::w_ser;
SerCont TeBkUmLpqQwAsyncRouter::wcont_ser;

TeBkUmLpqQwAsyncRouter::TeBkUmLpqQwAsyncRouter() {
	sqli = NULL;
	tmplFunc = TemplateUtil::getTemplateFunc("t5", "tpe/fortunes.tpe");
	m_ser = Serializer::getSerFuncForObject("t5", "TeBkUmLpqQwAsyncMessage");
	w_ser = Serializer::getSerFuncForObject("t5", "TeBkUmLpqQwAsyncWorld");
	wcont_ser = Serializer::getSerFuncForObjectCont("t5", "TeBkUmLpqQwAsyncWorld", "std::vector");
}

TeBkUmLpqQwAsyncRouter::~TeBkUmLpqQwAsyncRouter() {
	if(sqli!=NULL) {
		DataSourceManager::cleanRawImpl(sqli);
	}
}

LibpqDataSourceImpl* TeBkUmLpqQwAsyncRouter::getDb(int max) {
	if(sqli==NULL) {
		sqli = static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "t5"));
	}
	return sqli;
}

LibpqDataSourceImpl* TeBkUmLpqQwAsyncRouterPooled::getDb(int max) {
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
			pool.push_back(static_cast<LibpqDataSourceImpl*>(DataSourceManager::getRawImpl("PostgreSQL-DSN", "t5", true)));
		}
		inited = true;
	}
	return pool.at(pc%max);
}

TeBkUmLpqQwAsyncRouterPooled::TeBkUmLpqQwAsyncRouterPooled() {
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

TeBkUmLpqQwAsyncRouterPooled::~TeBkUmLpqQwAsyncRouterPooled() {
	for(auto sqli: pool) {
		if(sqli!=NULL) {
			DataSourceManager::cleanRawImpl(sqli);
		}
	}
}
