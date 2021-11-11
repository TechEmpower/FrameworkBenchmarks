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
 * TeBkUm.h
 *
 *  Created on: 03-Feb-2020
 *      Author: sumeetc
 */

#ifndef WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUm_H_
#define WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUm_H_
#include "TemplateHandler.h"
#include "vector"
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
#include "TeBkUmWorld.h"
#include "yuarel.h"
#include "Router.h"

#pragma @Entity
#pragma @Table name="fortune"
class TeBkUmFortune {
	#pragma @Id dbf="id"
	int id;
	#pragma @Column dbf="message"
	std::string message;
public:
	TeBkUmFortune();
	virtual ~TeBkUmFortune();
	int getId() const;
	void setId(int id);
	const std::string& getMessage() const;
	void setMessage(const std::string& message);
	bool operator < (const TeBkUmFortune& other) const;
};

class TeBkUmMessage {
	std::string message;
public:
	virtual ~TeBkUmMessage();
	const std::string& getMessage() const;
	void setMessage(const std::string& message);
};

class TeBkUmRouter : public Router {
	static const std::string HELLO_WORLD;
	static std::string WORLD;

	static TemplatePtr tmplFunc;

	static Ser m_ser;
	static Ser w_ser;
	static SerCont wcont_ser;

	bool strToNum(const char* str, int len, int& ret);
	void db(TeBkUmWorld&);
	void queries(const char*, int, std::vector<TeBkUmWorld>&);
	void updates(const char*, int, std::vector<TeBkUmWorld>&);
	void cachedWorlds(const char*, int, std::vector<TeBkUmWorld>&);
	void getContext(HttpRequest* request, Context* context);
public:
	TeBkUmRouter();
	virtual ~TeBkUmRouter();
	void updateCache();
	bool route(HttpRequest* req, HttpResponse* res, SocketInterface* sif);
};

#endif /* WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUm_H_ */
