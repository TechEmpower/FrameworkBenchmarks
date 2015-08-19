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
 * ConfiguartionData.h
 *
 *  Created on: 19-Jun-2012
 *      Author: sumeetc
 */

#ifndef CONFIGURATIONDATA_H_
#define CONFIGURATIONDATA_H_
#include "string"
#include "FFEADContext.h"
#include "RegexUtil.h"
#include "DataSourceMapping.h"
#include "Connection.h"
#include "CORSHandler.h"
#include "CommonUtils.h"
#include "SSLHandler.h"
#include "ClassStructure.h"
using namespace std;

typedef void* (*toObjectFromJson) (const string&);

class WsDetails {
	string claz;
	string location;
	string namespc;
	string appname;
	map<string, string> outnmmp;
	friend class ConfigurationHandler;
	friend class WsUtil;
};

class RestFunctionParams
{
	string name;
	string type;
	string from;
	string defValue;
	friend class ConfigurationHandler;
	friend class ControllerHandler;
};

class RestFunction
{
	string name;
	string path;
	string clas;
	string meth;
	string statusCode;
	string icontentType;
	string ocontentType;
	vector<RestFunctionParams> params;
	friend class ConfigurationHandler;
	friend class ControllerHandler;
};

typedef map<string, vector<RestFunction> > resFuncMap;

class SecureAspect
{
	string path;
	string role;
	friend class ConfigurationHandler;
	friend class ConfigurationData;
	friend class Security;
	friend class SecurityHandler;
};

class Security
{
	Logger logger;
	map<string, SecureAspect> secures;
	string loginProvider;
	string loginUrl;
	string welcomeFile;
	string name;
	long sessTimeout;
	map<string, string> securityFieldNames;
	map<string, string> securityFieldFrom;
	bool isLoginConfigured();
	bool isSecureConfigured();
	bool isLoginUrl(const string& cntxtName, const string& actUrl);
	bool isLoginPage(const string& cntxtName, const string& actUrl);
	SecureAspect matchesPath(const string& cntxtName, string actUrl);
	bool addAspect(const SecureAspect&);
	friend class ConfigurationData;
	friend class ConfigurationHandler;
	friend class SecurityHandler;
public:
	Security();
	~Security();
};

class UrlPattern {
	string pattern;
	int type;
	friend class ConfigurationHandler;
	friend class ConfigurationData;
	friend class Security;
	friend class SecurityHandler;
public:
	static enum {ANY, ANY_EXT, END_EXT, STARTSWITH, ENDSWITH, REGEX} PATTERN_TYPE;
};

class CoreServerProperties {
	string serverRootDirectory;
	string ip_address;
	string resourcePath;
	string webPath;
	long sessionTimeout;
	long sessionFileLockTimeout;
	bool sessatserv;
	bool sessservdistocache;
	bool isMainServerProcess;
	map<string, string> sprops;
	friend class ExtHandler;
	friend class SecurityHandler;
	friend class FormHandler;
	friend class SoapHandler;
	friend class CHServer;
	friend class ServiceTask;
	friend class ConfigurationHandler;
	friend class WsUtil;
	friend class DCPGenerator;
	friend class TemplateGenerator;
};

class ConfigurationData {
	ConfigurationData();
	static ConfigurationData* instance;
	map<string, string> appAliases;
	map<string, vector<string> > filterMap;
	map<string, map<string, vector<RestFunction> > > rstCntMap;
	map<string, string> handoffs;
	map<string, map<string, Security> > securityObjectMap;
	map<string, map<string, string> > controllerObjectMap;
	map<string, map<string, string> > mappingObjectMap;
	map<string, map<string, string> > mappingextObjectMap;
	map<string, map<string, vector<string> > > filterObjectMap;
	map<string, map<string, string> > viewMappingMap;
	map<string, map<string, string> > ajaxInterfaceMap;
	map<string, map<string, string> > fviewMappingMap;
	map<string, map<string, string> > wsdlMap;
	map<string, map<string, Element> > fviewFormMap;
	map<string, map<string, string> > templateMappingMap;
	map<string, map<string, string> > dcpMappingMap;
	map<string, map<string, string> > websocketMappingMap;
	map<string, string> dynamicCppPagesMap;
	map<string, string> templateFilesMap;
	map<string, bool> applicationFlowMap;
	map<string, bool> servingContexts;
	vector<string> componentNames;
	FFEADContext ffeadContext;
	CorsConfig corsConfig;
	SecurityProperties securityProperties;
	CoreServerProperties coreServerProperties;
	map<string, map<string, ConnectionProperties> > sdormConnProperties;
	map<string, map<string, Mapping> > sdormEntityMappings;
	map<string, map<string, ConnectionProperties> > cacheConnProperties;
	map<string, vector<WsDetails> > webserviceDetailMap;
	map<string, map<string, ClassStructure> > classStructureMap;
	Logger logger;
	ThreadLocal httpRequest;
	ThreadLocal httpResponse;
	static void clearInstance();
	friend class ExtHandler;
	friend class FilterHandler;
	friend class ConfigurationHandler;
	friend class ControllerHandler;
	friend class SecurityHandler;
	friend class CORSHandler;
	friend class FormHandler;
	friend class SoapHandler;
	friend class CHServer;
	friend class ServiceTask;
	friend class SSLHandler;
	friend class FviewHandler;
	friend class WsUtil;
	friend class DCPGenerator;
	friend class TemplateGenerator;
	friend class ApplicationUtil;
	friend class FFEADContext;
	friend class SocketUtil;
	static bool urlMatchesPath(const string& cntxtName, string pathurl, string url);
public:
	static bool isServingContext(const string& cntxtName);
	static ConfigurationData* getInstance();
	static SecurityProperties const& getSecurityProperties();
	static CoreServerProperties const& getCoreServerProperties();
	static HttpRequest* getHttpRequest();
	static HttpResponse* getHttpResponse();
	virtual ~ConfigurationData();
};

#endif /* CONFIGURATIONDATA_H_ */
