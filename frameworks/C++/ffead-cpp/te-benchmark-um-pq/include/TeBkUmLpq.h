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
 * TeBkUmLpq.h
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */

#ifndef WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUmLpq_H_
#define WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUmLpq_H_
#include "TemplateHandler.h"
#include "vector"
#include "list"
#ifndef OS_MINGW
#include <netinet/in.h>
#include <arpa/inet.h>
#endif
#include "DataSourceManager.h"
#include <stdlib.h>
#include <algorithm>
#include "CryptoHandler.h"
#include "CastUtil.h"
#include <stdlib.h>
#include "CacheManager.h"
#include "HttpRequest.h"
#include "HttpResponse.h"
#include "JSONSerialize.h"
#include "string"
#include "yuarel.h"
#include "Router.h"
#include "Reflector.h"
#include <unordered_map>

class TeBkUmLpqWorld {
	int id;
	int randomNumber;
public:
	TeBkUmLpqWorld(int id);
	TeBkUmLpqWorld(int id, int randomNumber);
	TeBkUmLpqWorld();
	virtual ~TeBkUmLpqWorld();
	int getId() const;
	void setId(int id);
	int getRandomNumber() const;
	void setRandomNumber(int randomNumber);
	bool operator < (const TeBkUmLpqWorld& other) const;
};

struct UpdQrData {
	std::vector<TeBkUmLpqWorld>* wlist;
	std::stringstream ss;
	bool status;
	int queryCount;
};

class TeBkUmLpqFortune {
	int id;
public:
	std::string message_i;
	std::string_view message;
	bool allocd;
	TeBkUmLpqFortune(int id);
	TeBkUmLpqFortune(int id, std::string message);
	TeBkUmLpqFortune();
	virtual ~TeBkUmLpqFortune();
	int getId() const;
	void setId(int id);
	bool operator < (const TeBkUmLpqFortune& other) const;
};

class TeBkUmLpqRouterPicoV;

class TeBkUmLpqMessage {
	std::string message;
public:
	TeBkUmLpqMessage();
	TeBkUmLpqMessage(std::string message);
	virtual ~TeBkUmLpqMessage();
	const std::string& getMessage() const;
	void setMessage(const std::string& message);
};

class TeBkUmLpqRouter : public Router {
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

	void db(TeBkUmLpqWorld&);
	void queries(const char*, int, std::vector<TeBkUmLpqWorld>&);
	void queriesMulti(const char*, int, std::vector<TeBkUmLpqWorld>&);
	void updates(const char*, int, std::vector<TeBkUmLpqWorld>&);
	void updatesMulti(const char*, int, std::vector<TeBkUmLpqWorld>&);
	void cachedWorlds(const char*, int, std::vector<TeBkUmLpqWorld>&);
	void handleTemplate(HttpRequest* req, HttpResponse* res, SocketInterface* sif);

	std::unordered_map<int, std::string> _qC;
	LibpqDataSourceImpl* sqli;
	LibpqDataSourceImpl* getDb();

	std::string& getUpdQuery(int count);
	friend class TeBkUmLpqRouterPicoV;
public:
	TeBkUmLpqRouter();
	virtual ~TeBkUmLpqRouter();
	void updateCache();
	bool route(HttpRequest* req, HttpResponse* res, SocketInterface* sif);
};

class TeBkUmLpqRouterPicoV : public TeBkUmLpqRouter {
	void handleTemplate(HttpResponse* res);
public:
	TeBkUmLpqRouterPicoV();
	virtual ~TeBkUmLpqRouterPicoV();
	bool route(HttpRequest* req, HttpResponse* res, SocketInterface* sif);
};

#endif /* WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUmLpq_H_ */
