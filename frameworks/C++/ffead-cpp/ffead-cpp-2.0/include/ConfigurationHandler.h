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
 * ConfigurationHandler.h
 *
 *  Created on: 19-Jun-2012
 *      Author: sumeetc
 */

#ifndef CONFIGURATIONHANDLER_H_
#define CONFIGURATIONHANDLER_H_
#include "AppDefines.h"
#include "TemplateEngine.h"
#include "Reflection.h"
#include "XmlParser.h"
#ifdef INC_COMP
#include "ComponentGen.h"
#endif
#ifdef INC_APPFLOW
#include "ApplicationUtil.h"
#endif
#ifdef INC_DCP
#include "DCPGenerator.h"
#endif
#include "ConfigurationData.h"
#include "LoggerFactory.h"
#ifdef INC_TPE
#include "TemplateGenerator.h"
#endif
#include "FFEADContext.h"
#ifdef INC_WEBSVC
#include "WsUtil.h"
#endif
#ifdef INC_JOBS
#include "JobScheduler.h"
#endif
#include "SSLHandler.h"
#include "ScriptHandler.h"
#include "fstream"
#include "iostream"
#include "DataSourceManager.h"
#include "CacheManager.h"
#include "CommonUtils.h"

class ConfigurationHandler {
	static Marker getRestFunctionMarker(map<string, vector<Marker> >& markers);
	static Marker getRestFunctionParamMarker(map<string, vector<Marker> >& markers);
	static void handleRestControllerMarker(ClassStructure& cs, const string& appName);
	static void handleMarkerConfigurations(map<string, map<string, ClassStructure> >& clsstrucMaps, vector<WsDetails>& wsdvec, vector<bool>& stat, strVec& vecvp, strVec& pathvec, map<string, string>& ajintpthMap, map<string, string>& tpes, const string& serverRootDirectory, strVec& afcd, string& ajrt, Reflection& ref);
	static void handleDataSourceEntities(const string& appName, map<string, Mapping>& mappings, map<string, ClassStructure>& allclsmap);
public:
	ConfigurationHandler();
	virtual ~ConfigurationHandler();
	static void handle(strVec webdirs, const strVec& webdirs1, const string& incpath, const string& rtdcfpath, const string& serverRootDirectory, const string& respath);
	//static void listi(const string& cwd, const string& type, const bool& apDir, strVec &folders, const bool& showHidden);
	static void configureDataSources(const string& name, const string& configFile, map<string, ClassStructure>& allclsmap);
	static void destroyDataSources();
	static void configureCaches(const string& name, const string& configFile);
	static void destroyCaches();
	static void initializeDataSources();
	static void initializeCaches();
	static void initializeWsdls();
};

#endif /* CONFIGURATIONHANDLER_H_ */
