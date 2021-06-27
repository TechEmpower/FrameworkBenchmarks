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
 * TeBkUmMgrUm.cpp
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */
#include "TeBkUmMgr.h"

int TeBkUmMgrWorld::getId() const {
	return id;
}

void TeBkUmMgrWorld::setId(int id) {
	this->id = id;
}

int TeBkUmMgrWorld::getRandomNumber() const {
	return randomNumber;
}

void TeBkUmMgrWorld::setRandomNumber(int randomNumber) {
	this->randomNumber = randomNumber;
}

TeBkUmMgrWorld::TeBkUmMgrWorld() {
	id = 0;
	randomNumber = 0;
}

TeBkUmMgrWorld::~TeBkUmMgrWorld() {
}

int TeBkUmMgrFortune::getId() const {
	return id;
}

void TeBkUmMgrFortune::setId(int id) {
	this->id = id;
}

const std::string& TeBkUmMgrFortune::getMessage() const {
	return message;
}

void TeBkUmMgrFortune::setMessage(const std::string& message) {
	this->message = message;
}

TeBkUmMgrFortune::TeBkUmMgrFortune() {
	id = 0;
}

TeBkUmMgrFortune::~TeBkUmMgrFortune() {
}

bool TeBkUmMgrFortune::operator < (const TeBkUmMgrFortune& other) const {
	return message.compare(other.message)<0;
}

TeBkUmMgrMessage::~TeBkUmMgrMessage() {
}

const std::string& TeBkUmMgrMessage::getMessage() const {
	return message;
}

void TeBkUmMgrMessage::setMessage(const std::string& message) {
	this->message = message;
}

const std::string TeBkUmMgrRouter::HELLO_WORLD = "Hello, World!";
std::string TeBkUmMgrRouter::WORLD = "world";
std::string TeBkUmMgrRouter::FORTUNE = "fortune";

void TeBkUmMgrRouter::db(TeBkUmMgrWorld& w) {
#ifdef INC_SDORM_MONGO
	MongoDBRawDataSourceImpl* sqli = getDb();
	int rid = rand() % 10000 + 1;
	try {
		bson_t q = BSON_INITIALIZER;
		bson_append_int32(&q, "_id", 3, rid);
		sqli->begin(WORLD);
		sqli->executeQuery(&q, &w, &TeBkUmMgrRouter::dbUtil);
		sqli->end();
		bson_destroy(&q);
	} catch(const std::exception& e) {
		throw e;
	}
#endif
}
#ifdef INC_SDORM_MONGO
void TeBkUmMgrRouter::dbUtil(void* ctx, int rn, std::vector<MgRawRes>& data) {
	TeBkUmMgrWorld* w = (TeBkUmMgrWorld*)ctx;
	for(int i=0;i<(int)data.size();i++) {
		if(data.at(i).n=="_id") {
			w->setId((int)data.at(i).d);
			if(w->getId()<=0) {
				w->setId((int)data.at(i).l);
			}
		}
		if(data.at(i).n=="randomNumber") {
			w->setRandomNumber((int)data.at(i).d);
			if(w->getRandomNumber()==0) {
				w->setRandomNumber((int)data.at(i).l);
			}
		}
	}
}
#endif

void TeBkUmMgrRouter::queries(const char* q, int ql, std::vector<TeBkUmMgrWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;
#ifdef INC_SDORM_MONGO
	MongoDBRawDataSourceImpl* sqli = getDb();

	try {
		TeBkUmMgrWorld w;
		sqli->begin(WORLD);
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			bson_t q = BSON_INITIALIZER;
			bson_append_int32(&q, "_id", 3, rid);
			sqli->executeQuery(&q, &w, &TeBkUmMgrRouter::dbUtil);
			bson_destroy(&q);
			wlst.push_back(w);
		}
		sqli->end();
	} catch(const std::exception& e) {
		throw e;
	}
#endif
}

void TeBkUmMgrRouter::updates(const char* q, int ql, std::vector<TeBkUmMgrWorld>& wlst) {
	int queryCount = 0;
	strToNum(q, ql, queryCount);
	if(queryCount<1)queryCount=1;
	else if(queryCount>500)queryCount=500;
#ifdef INC_SDORM_MONGO
	MongoDBRawDataSourceImpl* sqli = getDb();

	try {
		sqli->startBulk(WORLD);
		for (int c = 0; c < queryCount; ++c) {
			int rid = rand() % 10000 + 1;
			bson_t q;
			bson_init(&q);
			bson_append_int32(&q, "_id", 3, rid);
			TeBkUmMgrWorld w;
			sqli->executeQuery(&q, &w, &TeBkUmMgrRouter::dbUtil);
			int newRandomNumber = rand() % 10000 + 1;
			if(w.getRandomNumber() == newRandomNumber) {
				newRandomNumber += 1;
				if(newRandomNumber>=10000) {
					newRandomNumber = 1;
				}
			}
			w.setRandomNumber(newRandomNumber);
			bson_t du;
			bson_t d;
			bson_init(&du);
			bson_append_document_begin(&du, "$set", 4, &d);
			//bson_append_int32(&d, "_id", 3, w.getId());
			bson_append_int32(&d, "randomNumber", 12, w.getRandomNumber());
			bson_append_document_end(&du, &d);
			sqli->addBulk(&q, &du);
			/*char* str = bson_as_json(&du, NULL);
			printf("%s\n", str);
			bson_free(str);*/
			bson_destroy(&du);
			bson_destroy(&q);
			wlst.push_back(w);
		}
		sqli->endBulk();
	} catch(const std::exception& e) {
		throw e;
	}
#endif
}

void TeBkUmMgrRouter::updateCache() {
#ifdef INC_SDORM_MONGO
	CacheInterface* cchi = CacheManager::getImpl();
	MongoDBRawDataSourceImpl* sqli = getDb();

	try {
		std::vector<TeBkUmMgrWorld> wlist;
		sqli->begin(WORLD);
		sqli->executeQuery(NULL, &wlist, &TeBkUmMgrRouter::updateCacheUtil);
		sqli->end();

		for (int c = 0; c < (int)wlist.size(); ++c) {
			TeBkUmMgrWorld& w = wlist.at(c);
			char str[12];
			sprintf(str, "%d;%d", w.getId(), w.getRandomNumber());
			cchi->setRaw(CastUtil::fromNumber(w.getId()), str);
		}
		CacheManager::cleanImpl(cchi);
		CacheManager::triggerAppInitCompletion();
	} catch(const std::exception& e) {
		CacheManager::cleanImpl(cchi);
		throw e;
	}
#endif
}
#ifdef INC_SDORM_MONGO
void TeBkUmMgrRouter::updateCacheUtil(void* ctx, int rn, std::vector<MgRawRes>& data) {
	std::vector<TeBkUmMgrWorld>* wlist = (std::vector<TeBkUmMgrWorld>*)ctx;
	TeBkUmMgrWorld w;
	for(int i=0;i<(int)data.size();i++) {
		if(data.at(i).n=="_id") {
			w.setId((int)data.at(i).d);
			if(w.getId()<=0) {
				w.setId((int)data.at(i).l);
			}
		}
		if(data.at(i).n=="randomNumber") {
			w.setRandomNumber((int)data.at(i).d);
			if(w.getRandomNumber()<=0) {
				w.setRandomNumber((int)data.at(i).l);
			}
		}
	}
	wlist->push_back(w);
}
#endif

void TeBkUmMgrRouter::cachedWorlds(const char* q, int ql, std::vector<TeBkUmMgrWorld>& wlst) {
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

		std::vector<std::string> values;
		cchi->mgetRaw(keys, values);

		for (int c = 0; c < (int)values.size(); ++c) {
			TeBkUmMgrWorld w;
			std::string& v = values.at(c);
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

void TeBkUmMgrRouter::getContext(HttpRequest* request, Context* context) {
#ifdef INC_SDORM_MONGO
	MongoDBRawDataSourceImpl* sqli = getDb();

	try {
		std::vector<TeBkUmMgrFortune>* flst = new std::vector<TeBkUmMgrFortune>;
		sqli->begin(FORTUNE);
		sqli->executeQuery(NULL, flst, &TeBkUmMgrRouter::getContextUtil);
		sqli->end();

		TeBkUmMgrFortune nf;
		nf.setId(0);
		nf.setMessage("Additional fortune added at request time.");
		flst->push_back(nf);
		std::sort (flst->begin(), flst->end());

		context->insert(std::pair<std::string, void*>("fortunes", flst));
	} catch(...) {
		throw;
	}
#endif
}
#ifdef INC_SDORM_MONGO
void TeBkUmMgrRouter::getContextUtil(void* ctx, int rn, std::vector<MgRawRes>& data) {
	std::vector<TeBkUmMgrFortune>* flst = (std::vector<TeBkUmMgrFortune>*)ctx;
	TeBkUmMgrFortune w;
	for(int i=0;i<(int)data.size();i++) {
		if(data.at(i).n=="_id") {
			w.setId((int)data.at(i).d);
			if(w.getId()<=0) {
				w.setId((int)data.at(i).l);
			}
		}
		if(data.at(i).n=="message") {
			std::string nm = data.at(i).s;
			CryptoHandler::sanitizeHtml(nm);
			w.setMessage(nm);
		}
	}
	flst->push_back(w);
}
#endif

//https://stackoverflow.com/questions/9631225/convert-strings-specified-by-length-not-nul-terminated-to-int-float
bool TeBkUmMgrRouter::strToNum(const char* str, int len, int& ret) {
    ret = 0;
    for(int i = 0; i < len; ++i)
    {
    	if(!isdigit(str[i])) return false;
        ret = ret * 10 + (str[i] - '0');
    }
    return true;
}

bool TeBkUmMgrRouter::route(HttpRequest* req, HttpResponse* res, SocketInterface* sif) {
	std::string_view path = req->getPath();
	if(StringUtil::endsWith(path, "/plaintext")) {
		res->setContent(HELLO_WORLD);
		res->setContentType(ContentTypes::CONTENT_TYPE_TEXT_PLAIN);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/json")) {
		TeBkUmMgrMessage msg;
		msg.setMessage(HELLO_WORLD);
		JSONSerialize::serializeObject(&msg, m_ser, res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/db")) {
		TeBkUmMgrWorld msg;
		db(msg);
		JSONSerialize::serializeObject(&msg, w_ser, res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/queries")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmMgrWorld> msg;
		queries(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/fortunes")) {
		Context ctx;
		getContext(req, &ctx);

		if(tmplFunc!=NULL)
		{
			fcpstream str;
			tmplFunc(&ctx, str);
			res->setContent(str.str());
			res->setContentType(ContentTypes::CONTENT_TYPE_TEXT_SHTML);
			res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
		}
	} else if(StringUtil::endsWith(path, "/updates")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmMgrWorld> msg;
		updates(params[0].val, params[0].val_len, msg);
		JSONSerialize::serializeObjectCont(&msg, wcont_ser, "vector", res->getContentP());
		res->setContentType(ContentTypes::CONTENT_TYPE_APPLICATION_JSON);
		res->setHTTPResponseStatus(HTTPResponseStatus::Ok);
	} else if(StringUtil::endsWith(path, "/cached-worlds")) {
		struct yuarel_param params[1];
		yuarel_parse_query((char*)req->getQueryStr().data(), req->getQueryStr().size(), params, 1);
		std::vector<TeBkUmMgrWorld> msg;
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

TemplatePtr TeBkUmMgrRouter::tmplFunc;
Ser TeBkUmMgrRouter::m_ser;
Ser TeBkUmMgrRouter::w_ser;
SerCont TeBkUmMgrRouter::wcont_ser;

TeBkUmMgrRouter::TeBkUmMgrRouter() {
#ifdef INC_SDORM_MONGO
	sqli = NULL;
#endif
	tmplFunc = TemplateUtil::getTemplateFunc("te-benchmark-um-mgr", "tpe/fortunes.tpe");
	m_ser = Serializer::getSerFuncForObject("te-benchmark-um-mgr", "TeBkUmMgrMessage");
	w_ser = Serializer::getSerFuncForObject("te-benchmark-um-mgr", "TeBkUmMgrWorld");
	wcont_ser = Serializer::getSerFuncForObjectCont("te-benchmark-um-mgr", "TeBkUmMgrWorld", "std::vector");
}

TeBkUmMgrRouter::~TeBkUmMgrRouter() {
}

#ifdef INC_SDORM_MONGO
MongoDBRawDataSourceImpl* TeBkUmMgrRouter::getDb() {
	if(sqli==NULL) {
		sqli = static_cast<MongoDBRawDataSourceImpl*>(DataSourceManager::getRawImpl("MongoDB-DSN", "te-benchmark-um-mgr"));
	}
	return sqli;
}
#endif
