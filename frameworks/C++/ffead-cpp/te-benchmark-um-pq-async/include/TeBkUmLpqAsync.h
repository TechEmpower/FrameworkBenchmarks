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
 * TeBkUmLpqAsync.h
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */

#ifndef WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUmLpqAsync_H_
#define WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUmLpqAsync_H_
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
#include "CacheManager.h"
#include <stdlib.h>
#include "HttpRequest.h"
#include "HttpResponse.h"
#include "JSONSerialize.h"
#include "string"
#include "yuarel.h"
#include "Router.h"

class TeBkUmLpqAsyncWorld {
	int id;
	int randomNumber;
public:
	TeBkUmLpqAsyncWorld();
	TeBkUmLpqAsyncWorld(int id);
	TeBkUmLpqAsyncWorld(int id, int randomNumber);
	virtual ~TeBkUmLpqAsyncWorld();
	int getId() const;
	void setId(int id);
	int getRandomNumber() const;
	void setRandomNumber(int randomNumber);
};

class TeBkUmLpqAsyncFortune {
	int id;
public:
	std::string message_i;
	std::string_view message;
	bool allocd;
	TeBkUmLpqAsyncFortune(int id);
	TeBkUmLpqAsyncFortune(int id, std::string message);
	TeBkUmLpqAsyncFortune();
	virtual ~TeBkUmLpqAsyncFortune();
	int getId() const;
	void setId(int id);
	bool operator < (const TeBkUmLpqAsyncFortune& other) const;
};

class TeBkUmLpqAsyncMessage {
	std::string message;
public:
	TeBkUmLpqAsyncMessage();
	TeBkUmLpqAsyncMessage(std::string message);
	virtual ~TeBkUmLpqAsyncMessage();
	const std::string& getMessage() const;
	void setMessage(const std::string& message);
};

struct AsyncReq {
	float httpVers;
	bool conn_clos;
	SocketInterface* sif;
	LibpqDataSourceImpl* sqli;

	TeBkUmLpqAsyncWorld w;
	std::vector<TeBkUmLpqAsyncWorld> vec;
	std::list<TeBkUmLpqAsyncFortune> flst;
};

struct CacheReq {
	CacheInterface* cchi;

	std::vector<TeBkUmLpqAsyncWorld> vec;
};

class TeBkUmLpqAsyncRouter : public Router {
	static const std::string HELLO_WORLD;
	static std::string WORLD;
	static std::string WORLD_ONE_QUERY;
	static std::string WORLD_ALL_QUERY;
	static std::string FORTUNE_ALL_QUERY;

	static TemplatePtr tmplFunc;

	static Ser m_ser;
	static Ser w_ser;
	static SerCont wcont_ser;

	static bool strToNum(const char* str, int len, int& ret);

	void dbAsync(AsyncReq* req);
	static void dbAsyncUtil(void* ctx, int rn, int cn, char * d);
	static void dbAsyncCh(void* ctx, bool status, const std::string& q, int counter);

	void queriesAsync(const char* q, int ql, AsyncReq* req);
	static void queriesAsyncUtil(void* ctx, int rn, int cn, char * d);
	static void queriesAsyncCh(void* ctx, bool status, const std::string& q, int counter);

#ifndef HAVE_LIBPQ_BATCH
	void queriesMultiAsync(const char*, int, AsyncReq*);
	static void queriesMultiAsyncUtil(void* ctx, int, int, char *, int);
	static void queriesMultiAsyncCh(void* ctx, bool status, const std::string& q, int counter);
#endif

	void updatesAsync(const char* q, int ql, AsyncReq* req);
	static void updatesAsyncChQ(void* ctx, bool status, const std::string& q, int counter);
	static void updatesAsyncChU(void* ctx, bool status, const std::string& q, int counter);

	static std::string& getUpdQuery(int count);
	void updatesAsyncb(const char* q, int ql, AsyncReq* req);
	static void updatesAsyncbChQ(void* ctx, bool status, const std::string& q, int counter);
	static void updatesAsyncbChU(void* ctx, bool status, const std::string& q, int counter);
	
	void cachedWorlds(const char*, int, std::vector<TeBkUmLpqAsyncWorld>&);
	static void updateCacheAsyncUtil(void* ctx, int rn, std::vector<LibpqRes>& data);
	static void updateCacheAsyncCh(void* ctx, bool status, const std::string& q, int counter);

	void getContextAsync(AsyncReq* req);
	static void getContextAsyncUtil(void* ctx, int rn, int cn, char * d, int l);
	static void getContextAsyncCh(void* ctx, bool status, const std::string& q, int counter);

	static std::map<int, std::string> _qC;
	LibpqDataSourceImpl* sqli;
	LibpqDataSourceImpl* getDb();
	//static Logger logger;
public:
	TeBkUmLpqAsyncRouter& operator=(const TeBkUmLpqAsyncRouter& a) {
		return *this;
	}
	TeBkUmLpqAsyncRouter(const TeBkUmLpqAsyncRouter& a) {
		sqli = NULL;
	}
	TeBkUmLpqAsyncRouter();
	virtual ~TeBkUmLpqAsyncRouter();
	void updateCache();
	bool route(HttpRequest* req, HttpResponse* res, SocketInterface* sif);
};

#endif /* WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUmLpqAsync_H_ */
