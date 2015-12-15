/*
	Copyright 2010, Sumeet Chhetri

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
 * LoggerFactory.h
 *
 *  Created on: 16-Jul-2013
 *      Author: sumeetc
 */

#ifndef LOGGERFACTORY_H_
#define LOGGERFACTORY_H_
#include "Logger.h"
#include "XmlParser.h"
#include "CommonUtils.h"

class LoggerFactory {
	map<string, LoggerConfig*> configs;
	map<string, string> dupLogNames;
	int vhostNumber;
	static LoggerFactory* instance;
	LoggerFactory();
	static void setVhostNumber(const int& vhn);
	static void init();
	static void configureDefaultLogger(const string& appName);
	friend class CHServer;
public:
	virtual ~LoggerFactory();
	static void clear();
	static void init(const string& configFile, const string& serverRootDirectory, const string& appName = "", const bool& isLoggingEnabled = true);
	static Logger getLogger(const string& className);
	static Logger getLogger(const string& loggerName, const string& className);
};

#endif /* LOGGERFACTORY_H_ */
