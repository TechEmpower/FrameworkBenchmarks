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
 * ControllerHandler.h
 *
 *  Created on: Jun 17, 2012
 *      Author: Sumeet
 */

#ifndef CONTROLLERHANDLER_H_
#define CONTROLLERHANDLER_H_
#include "AppDefines.h"
#include "Controller.h"
#include "Constants.h"
#include "LoggerFactory.h"
#ifdef INC_XMLSER
#include "XMLSerialize.h"
#endif
#include "JSONSerialize.h"
#include "ConfigurationData.h"

class ControllerHandler {
	static bool getControllerForPath(const string& cntxtName, const string& actUrl, string& className);
	static bool getMappingForPath(const string& cntxtName, const string& actUrl, string& to);
public:
	static bool handle(HttpRequest* req, HttpResponse* res, const string& ext, const string& pthwofile, Reflector& reflector);
};

#endif /* CONTROLLERHANDLER_H_ */
