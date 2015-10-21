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
 * SecurityHandler.h
 *
 *  Created on: Jun 17, 2012
 *      Author: Sumeet
 */

#ifndef SECURITYHANDLER_H_
#define SECURITYHANDLER_H_
#include "FileAuthController.h"
#include "Reflector.h"
#include "HttpRequest.h"
#include "HttpResponse.h"
#include "LoggerFactory.h"
#include "ConfigurationData.h"

typedef ClassInfo (*FunPtr) ();

class SecurityHandler {
	static string isLoginPage(const string& cntxtName, const string& actUrl);
	static string validateSecurePath(const string& cntxtName, const string& actUrl, const string& username);
	static void populateAuthDetails(HttpRequest* req);
	friend class ServiceTask;
public:
	SecurityHandler();
	virtual ~SecurityHandler();
	static bool handle(HttpRequest* req, HttpResponse* res, const long& sessionTimeout, Reflector& reflector);
};

#endif /* SECURITYHANDLER_H_ */
