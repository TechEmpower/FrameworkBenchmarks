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
 * Logger.h
 *
 *  Created on: Sep 5, 2009
 *      Author: sumeet
 */

#ifndef LOGGER_H_
#define LOGGER_H_
#include "PropFileReader.h"
#include "DateFormat.h"
#include "Mutex.h"
#include "RegexUtil.h"

class LoggerConfig
{
	string name, mode, level, file, logdirtype, pattern;
	Mutex lock;
	ostream* out;
	DateFormat datFormat;
	int vhostNumber;
	friend class LoggerFactory;
	friend class Logger;
};

class Logger {
public:
	static string LEVEL_OFF;
	static string LEVEL_FATAL;
	static string LEVEL_ERROR;
	static string LEVEL_WARN;
	static string LEVEL_INFO;
	static string LEVEL_DEBUG;
	static string LEVEL_TRACE;
	void fatal(const string&);
	void error(const string&);
	void warn(const string&);
	void info(const string&);
	void debug(const string&);
	void trace(const string&);
	Logger();
	virtual ~Logger();
	Logger& f();
	Logger& e();
	Logger& w();
	Logger& i();
	Logger& d();
	Logger& t();
	template <typename T>
	friend Logger& operator<< (Logger& logger, const T& msg)
	{
		logger.write(msg, logger.level, false);
		return logger;
	}
	friend Logger& operator<< (Logger& logger, ostream& (*pf) (ostream&));
private:
	static map<string, int> levelMap;
	void setClassName(const string& className);
	friend class CHServer;
	friend class LoggerFactory;
	Logger(LoggerConfig *config, const string& className);
	Logger(LoggerConfig *config, const string& className, const string& level);
	string className, level;
	LoggerConfig *config;
	void write(const string& msg, const string& mod, const bool& newline);
	template <typename T>
	void write(const T& tmsg, const string& mod, const bool& newline)
	{
		if(config==NULL)return;
		Date dat;
		string te = config->datFormat.format(dat);
		string vhnclsn = this->className + (config->vhostNumber>0?("-"+CastUtil::lexical_cast<string>(config->vhostNumber)):"");
		string msg = "[" + te + "] ("+vhnclsn + ") <"+mod+"> :";
		config->lock.lock();
		*config->out << msg << tmsg;
		if(newline)
		{
			*config->out << endl;
		}
		else
		{
			*config->out << flush;
		}
		config->lock.unlock();
	}
	void write(ostream& (*pf) (ostream&), const string& mod);
};
#endif /* LOGGER_H_ */
