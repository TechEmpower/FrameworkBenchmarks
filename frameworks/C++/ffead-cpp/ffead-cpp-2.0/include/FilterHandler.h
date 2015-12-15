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
 * FilterHandler.h
 *
 *  Created on: Jun 17, 2012
 *      Author: Sumeet
 */

#ifndef FILTERHANDLER_H_
#define FILTERHANDLER_H_
#include "Filter.h"
#include "Reflector.h"
#include <dlfcn.h>
#include "LoggerFactory.h"
#include "ConfigurationData.h"

typedef ClassInfo (*FunPtr) ();

class FilterHandler {
	static bool getFilterForPath(const string& cntxtName, const string& actUrl, vector<string>& filters, const string& type);
public:
	static void handleIn(HttpRequest* req, const string& ext, Reflector& reflector);
	static bool handle(HttpRequest* req, HttpResponse* res, const string& ext, Reflector& reflector);
	static void handleOut(HttpRequest* req, HttpResponse* res, const string& ext, Reflector& reflector);
};

#endif /* FILTERHANDLER_H_ */
