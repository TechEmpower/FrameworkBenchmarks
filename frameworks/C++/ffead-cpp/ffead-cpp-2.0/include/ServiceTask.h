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
 * ServiceTask.h
 *
 *  Created on: 20-Jun-2012
 *      Author: sumeetc
 */

#ifndef SERVICETASK_H_
#define SERVICETASK_H_
#include "AppDefines.h"
#include <sys/stat.h>
#include <math.h>
#include "DateFormat.h"
#include "HttpServiceHandler.h"
#include "SSLHandler.h"
#include "StringUtil.h"
#include "ConfigurationHandler.h"
#include "FormController.h"
#include "SecurityHandler.h"
#include "FilterHandler.h"
#include "ControllerHandler.h"
#include "FormHandler.h"
#include "Thread.h"
#ifdef INC_WEBSVC
#include "SoapHandler.h"
#endif
#include "ScriptHandler.h"
#include "FviewHandler.h"
#include "ExtHandler.h"
#include "CORSHandler.h"
#include "LoggerFactory.h"
#include "Timer.h"
#include <fcntl.h>
#ifdef INC_DSTC
#include "DistGlobalCache.h"
#endif
#include <stdio.h>
#include "Http11WebSocketDataFrame.h"
#include "CommonUtils.h"
#include "SocketUtil.h"

class ServiceTask : public HttpServiceTask
{
	Logger logger;
	int fd;
	string serverRootDirectory;
	map<string,string> *params;
	bool isSSLEnabled;
	SSL_CTX *ctx;
	void saveSessionDataToFile(const string& sessionId, const string& value);
	map<string,string> getSessionDataFromFile(const string& sessionId);
	void saveSessionDataToDistocache(const string& sessionId, map<string,string>& sessAttrs);
	map<string,string> getSessionDataFromDistocache(const string& sessionId);
	void storeSessionAttributes(HttpResponse* res, HttpRequest* req, const long& sessionTimeout, const bool& sessatserv);
	void updateContent(HttpRequest* req, HttpResponse *res, const string& ext, const int&);
	unsigned int getFileSize(const char *fileName);
	string getFileContents(const char *fileName, const int& start= -1, const int& end= -1);
	bool checkSocketWaitForTimeout(const int& sock_fd, const int& writing, const int& seconds, const int& micros= 0);
	/*bool sendData(SSL* ssl, BIO* io, const int& fd, const string& h1);
	void closeSocket(SSL* ssl, BIO* io, const int& fd);
	bool readLine(SSL* ssl, BIO* io, const int& fd, string& line);
	bool readData(SSL* ssl, BIO* io, const int& fd, const int& cntlen, string& content);
	*/
	void handleWebSocket(HttpRequest* req, void* dlib, void* ddlib, SocketUtil* sockUtil);
public:
	ServiceTask(const int& fd, const string& serverRootDirectory);
	ServiceTask();
	virtual ~ServiceTask();
	void handleWebsockOpen(const string& url);
	void handleWebsockClose(const string& url);
	void handleWebsockMessage(const string& url, WebSocketData* request, WebSocketData* response);
	void handle(HttpRequest* req, HttpResponse* res);
	void run_old();
	HttpResponse apacheRun(HttpRequest* req);
};

#endif /* SERVICETASK_H_ */
