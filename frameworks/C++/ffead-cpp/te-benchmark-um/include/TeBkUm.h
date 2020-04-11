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

typedef std::string (*TeBkUmTemplatePtr) (Context*);

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

#pragma @Router
class TeBkUmRouter : public Router {
	static const std::string HELLO_WORLD;
	static std::string WORLD;
	bool strToNum(const char* str, int len, int& ret);
	void db(TeBkUmWorld&);
	void queries(const char*, int, std::vector<TeBkUmWorld>&);
	void updates(const char*, int, std::vector<TeBkUmWorld>&);
	void cachedWorlds(const char*, int, std::vector<TeBkUmWorld>&);
	void getContext(HttpRequest* request, Context* context);
public:
	void updateCache();
	void route(HttpRequest* req, HttpResponse* res, void* dlib, void* ddlib);
};

#endif /* WEB_TE_BENCHMARK_UM_INCLUDE_TeBkUm_H_ */
