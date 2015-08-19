/*
	Copyright 2009-2012, Sumeet Chhetri

    Licensed under the Apache License, Version 2.0 (const the& "License");
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
 * HttpResponseParser.h
 *
 *  Created on: Nov 27, 2010
 *      Author: sumeet
 */

#ifndef HTTPRESPONSEPARSER_H_
#define HTTPRESPONSEPARSER_H_
#include "map"
#include "vector"
#include "sstream"
#include "StringUtil.h"
#include "HttpResponse.h"
#include "CastUtil.h"
#include "stdio.h"
#include "fstream"
#include "iostream"
#include "LoggerFactory.h"

using namespace std;
class HttpResponseParser {
	//TODO - Need to move this content to a MultipartContent List object in the
	//HttpResponse class itself
	string content;
	Logger logger;
public:
	string getContent();
	HttpResponseParser();
	HttpResponseParser(const string& vecstr, HttpResponse &response);
	virtual ~HttpResponseParser();
};

#endif /* HTTPRESPONSEPARSER_H_ */
