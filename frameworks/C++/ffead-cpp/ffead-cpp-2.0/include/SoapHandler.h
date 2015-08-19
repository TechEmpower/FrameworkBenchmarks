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
 * SoapHandler.h
 *
 *  Created on: Jun 17, 2012
 *      Author: Sumeet
 */

#ifndef SOAPHANDLER_H_
#define SOAPHANDLER_H_
#include "Reflector.h"
#include <dlfcn.h>
#include "HttpRequest.h"
#include "HttpResponse.h"
#include "Document.h"
#include "LoggerFactory.h"
#include "XmlParser.h"
#include "ConfigurationData.h"

class SoapHandler {
public:
	static void handle(HttpRequest* req, HttpResponse* res, void* dlib);
};

#endif /* SOAPHANDLER_H_ */
