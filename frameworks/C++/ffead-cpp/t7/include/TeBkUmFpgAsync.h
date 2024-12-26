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
 * TeBkUmFpgAsync.h
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */

#ifndef WEB_t1_INCLUDE_TeBkUmFpgAsync_H_
#define WEB_t1_INCLUDE_TeBkUmFpgAsync_H_
#include "TemplateHandler.h"
#include "vector"
#include "unordered_map"
#ifndef OS_MINGW
#include <netinet/in.h>
#include <arpa/inet.h>
#endif
#include "DataSourceManager.h"
#include <stdlib.h>
#include <algorithm>
#include "CryptoHandler.h"
#include "vector"
#include "CastUtil.h"
#include "CacheManager.h"
#include <stdlib.h>
#include "HttpRequest.h"
#include "HttpResponse.h"
#include "JSONSerialize.h"
#include "string"
#include "yuarel.h"
#include "Router.h"
#include <unordered_map>
#include "ConfigurationData.h"

class TeBkUmFpgAsyncWorld;

class TeBkUmFpgAsyncWorld {
	int id;
	int randomNumber;
	static TemplateJson tJ;
	static TemplateJson tJc;
public:
	TeBkUmFpgAsyncWorld();
	TeBkUmFpgAsyncWorld(int id);
	TeBkUmFpgAsyncWorld(int id, int randomNumber);
	virtual ~TeBkUmFpgAsyncWorld();
	int getId() const;
	void setId(int id);
	int getRandomNumber() const;
	void setRandomNumber(int randomNumber);
	static void tmplJson(int id, int randomNumber, std::string* ou) {
		size_t il = ou->length();
		ou->append(tJ.t);
		std::string ids = std::to_string(id);
		ou->insert(il+tJ.tpos.at(0), ids);
		ou->insert(il+tJ.tpos.at(1)+ids.length(), std::to_string(randomNumber));
	}
	void tmplJson(std::string* ou) {
		size_t il = ou->length();
		ou->append(tJ.t);
		std::string ids = std::to_string(id);
		ou->insert(il+tJ.tpos.at(0), ids);
		ou->insert(il+tJ.tpos.at(1)+ids.length(), std::to_string(randomNumber));
	}
	static void tmplJson(std::vector<TeBkUmFpgAsyncWorld>& vec, std::string* ou) {
		for(auto el: vec) {
			size_t il = ou->length();
			ou->append(tJc.t);
			std::string ids = std::to_string(el.id);
			ou->insert(il+tJc.tpos.at(0), ids);
			ou->insert(il+tJc.tpos.at(1)+ids.length(), std::to_string(el.randomNumber));
		}
		ou->insert(0, "[");
		ou->at(ou->length()-1) = ']';
	}
#ifdef HAVE_RAPID_JSON
	void toJson(rapidjson::Writer<rapidjson::StringBuffer>& w) {
		w.StartObject();
		w.String("id", 2);
	    w.Int(id);
		w.String("randomNumber", 12);
	    w.Int(randomNumber);
		w.EndObject();
	}
#endif
#ifdef HAVE_RAPID_JSON
	static void toJson(std::vector<TeBkUmFpgAsyncWorld>& vec, rapidjson::Writer<rapidjson::StringBuffer>& w) {
		w.StartArray();
		for(auto el: vec) {
			el.toJson(w);
		}
		w.EndArray();
	}
#endif
};

class TeBkUmFpgAsyncFortune {
	int id;
public:
	std::string message_i;
	std::string_view message;
	bool allocd;
	TeBkUmFpgAsyncFortune(int id);
	TeBkUmFpgAsyncFortune(int id, std::string message);
	TeBkUmFpgAsyncFortune(int id, const uint8_t * buf, size_t len);
	virtual ~TeBkUmFpgAsyncFortune();
	int getId() const;
	void setId(int id);
	bool operator < (const TeBkUmFpgAsyncFortune& other) const;
	TeBkUmFpgAsyncFortune() {
    	allocd = false;
    	id = 0;
    }
};

class TeBkUmFpgAsyncMessage {
	std::string message;
	static TemplateJson tJ;
public:
	TeBkUmFpgAsyncMessage();
	TeBkUmFpgAsyncMessage(std::string message);
	virtual ~TeBkUmFpgAsyncMessage();
	const std::string& getMessage() const;
	void setMessage(const std::string& message);
	void tmplJson(std::string* ou) {
		size_t il = ou->length();
		ou->append(tJ.t);
		ou->insert(il+tJ.tpos.at(0), message);
	}
#ifdef HAVE_RAPID_JSON
	void toJson(rapidjson::Writer<rapidjson::StringBuffer>& w) {
		w.StartObject();
		w.String("message", 7);
	    w.String(message.c_str(), static_cast<rapidjson::SizeType>(message.length()));
		w.EndObject();
	}
#endif
};

struct AsyncCacheReq1 {
	CacheInterface* cchi;
	std::vector<TeBkUmFpgAsyncWorld> vec;
};

class TeBkUmFpgAsyncRouter : public Router {
	static const std::string HELLO_WORLD;
	static const std::string WORLD;
	static const std::string WORLD_ONE_QUERY;
	static const std::string WORLD_ALL_QUERY;
	static const std::string FORTUNE_ALL_QUERY;
	static int g_seed;

	static TemplatePtr tmplFunc;

	static Ser m_ser;
	static Ser w_ser;
	static SerCont wcont_ser;

	void dbAsync(Writer* sif, HttpResponse* res);
	void queriesAsync(const char* q, int ql, Writer* sif, HttpResponse* res);
	void updatesAsync(const char* q, int ql, Writer* sif, HttpResponse* res);
	void updatesAsyncb(const char* q, int ql, Writer* sif, HttpResponse* res);
	void cachedWorlds(const char*, int, std::vector<TeBkUmFpgAsyncWorld>&);
	void fortunes(Writer* sif, HttpResponse* res);

	void queriesMultiAsync(const char*, int, Writer* sif, HttpResponse* res);
	void updatesMulti(const char*, int, Writer* sif, HttpResponse* res);

	static std::string& getUpdQuery(int count);
	static std::string& getMultiQuery(int count);

	static std::unordered_map<int, std::string> _qC;
	static std::unordered_map<int, std::string> _mqC;
	LibpqDataSourceImpl* sqli;
protected:
	virtual LibpqDataSourceImpl* getDb(int max = 0);
public:
	TeBkUmFpgAsyncRouter& operator=(const TeBkUmFpgAsyncRouter& a) {
		return *this;
	}
	TeBkUmFpgAsyncRouter(const TeBkUmFpgAsyncRouter& a) {
		sqli = NULL;
	}
	TeBkUmFpgAsyncRouter();
	virtual ~TeBkUmFpgAsyncRouter();
	void updateCache();
	/* These functions are here just for test purposes and serve no purpose START */
	static void temp() {
	}
	virtual void temp1() const {
	}
	std::map<std::string, std::string> l(std::map<std::string, std::string> a1, std::map<std::string, std::string> a2) {
		return std::map<std::string, std::string>();
	}
	/* END */
	bool route(HttpRequest* req, HttpResponse* res, Writer* sif);
	void routeAsync(HttpRequest* req, HttpResponse* res, Writer* sif);
};

class TeBkUmFpgAsyncRouterPooled : public TeBkUmFpgAsyncRouter {
	LibpqDataSourceImpl* getDb(int max = 0);
	std::atomic<int> opt;
	bool inited;
	int maxconns;
	std::vector<LibpqDataSourceImpl*> pool;
public:
	TeBkUmFpgAsyncRouterPooled& operator=(const TeBkUmFpgAsyncRouterPooled& a) {
		return *this;
	}
	TeBkUmFpgAsyncRouterPooled(const TeBkUmFpgAsyncRouterPooled& a) {
		this->opt = 0;
		this->inited = false;
		this->maxconns = 7;
	}
	TeBkUmFpgAsyncRouterPooled();
	virtual ~TeBkUmFpgAsyncRouterPooled();
};

#endif /* WEB_t1_INCLUDE_TeBkUmFpgAsync_H_ */
