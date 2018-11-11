/*
 * TeBkRestController.h
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#ifndef TEBKRESTCONTROLLER_H_
#define TEBKRESTCONTROLLER_H_
#include "TeBkWorld.h"
#include "vector"
#include "CastUtil.h"
#include "TeBkMessage.h"
#include "DataSourceManager.h"
#include <stdlib.h>
#include "CacheManager.h"


#pragma @RestController path="/"
class TeBkRestController {
public:
	#pragma @GET path="/json" statusCode="200" ocontentType="application/json"
	TeBkMessage json();

	#pragma @GET path="/db" statusCode="200" ocontentType="application/json"
	TeBkWorld db();

	#pragma @GET path="/queries" statusCode="200" ocontentType="application/json"
	std::vector<TeBkWorld> queries(
					#pragma @QueryParam name="queries"
					std::string queries);

	#pragma @GET path="/updates" statusCode="200" ocontentType="application/json"
	std::vector<TeBkWorld> updates(
					#pragma @QueryParam name="queries"
					std::string queries);

	#pragma @GET path="/plaintext" statusCode="200" ocontentType="text/plain"
	std::string plaintext();

	#pragma @GET path="/cached-worlds" statusCode="200" ocontentType="application/json"
	std::vector<TeBkWorld> cachedWorlds(
					#pragma @QueryParam name="count"
					std::string count);

	void updateCache();
	TeBkRestController();
	virtual ~TeBkRestController();
};

#endif /* TEBKRESTCONTROLLER_H_ */
