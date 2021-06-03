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
 * TeBkUmMgr.h
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */

#ifndef WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUmMgr_H_
#define WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUmMgr_H_
#include "TemplateHandler.h"
#include "vector"
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
#include <stdlib.h>
#include "CacheManager.h"
#include "HttpRequest.h"
#include "HttpResponse.h"
#include "JSONSerialize.h"
#include "string"
#include "yuarel.h"
#include "Router.h"

class TeBkUmMgrWorld {
	int id;
	int randomNumber;
public:
	TeBkUmMgrWorld();
	virtual ~TeBkUmMgrWorld();
	int getId() const;
	void setId(int id);
	int getRandomNumber() const;
	void setRandomNumber(int randomNumber);
};

class TeBkUmMgrFortune {
	int id;
	std::string message;
public:
	TeBkUmMgrFortune();
	virtual ~TeBkUmMgrFortune();
	int getId() const;
	void setId(int id);
	const std::string& getMessage() const;
	void setMessage(const std::string& message);
	bool operator < (const TeBkUmMgrFortune& other) const;
};

class TeBkUmMgrMessage {
	std::string message;
public:
	virtual ~TeBkUmMgrMessage();
	const std::string& getMessage() const;
	void setMessage(const std::string& message);
};

class TeBkUmMgrRouter : public Router {
	static const std::string HELLO_WORLD;
	static std::string WORLD;
	static std::string FORTUNE;

	static TemplatePtr tmplFunc;

	static Ser m_ser;
	static Ser w_ser;
	static SerCont wcont_ser;

	bool strToNum(const char* str, int len, int& ret);

	void db(TeBkUmMgrWorld&);
	void queries(const char*, int, std::vector<TeBkUmMgrWorld>&);
#ifdef INC_SDORM_MONGO
	static void dbUtil(void* ctx, int rn, std::vector<MgRawRes>& data);
#endif

	void updates(const char*, int, std::vector<TeBkUmMgrWorld>&);
	
	void cachedWorlds(const char*, int, std::vector<TeBkUmMgrWorld>&);
#ifdef INC_SDORM_MONGO
	static void updateCacheUtil(void* ctx, int rn, std::vector<MgRawRes>& data);
#endif

	void getContext(HttpRequest* request, Context* context);
#ifdef INC_SDORM_MONGO
	static void getContextUtil(void* ctx, int rn, std::vector<MgRawRes>& data);
	MongoDBRawDataSourceImpl* sqli;
	MongoDBRawDataSourceImpl* getDb();
#endif
public:
	TeBkUmMgrRouter();
	virtual ~TeBkUmMgrRouter();
	void updateCache();
	bool route(HttpRequest* req, HttpResponse* res, SocketInterface* sif);
};

#endif /* WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUmMgr_H_ */
