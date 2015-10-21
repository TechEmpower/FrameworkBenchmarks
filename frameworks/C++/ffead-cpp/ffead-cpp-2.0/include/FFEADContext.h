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
 * FFEADContext.h
 *
 *  Created on: Oct 17, 2010
 *      Author: root
 */

#ifndef FFEADCONTEXT_H_
#define FFEADCONTEXT_H_
#include "XmlParser.h"
#include "Reflector.h"
#include "CastUtil.h"
#include "StringUtil.h"
#include "LoggerFactory.h"

class Bean
{
	friend class FFEADContext;
	string name,inbuilt,value,clas,bean,intfType,injectAs,scope;
	bool realbean;
	vector<string> injs,names,types;
	string appName;
public:
	Bean();
	Bean(const string& name, const string& value, const string& clas, const string& scope, const bool& isInbuilt, const string& appName= "default");
	~Bean();
};
typedef map<string,Bean> beanMap;
class FFEADContext {
	Logger logger;
	beanMap beans,injbns;
	map<string,void*> objects;
	bool cleared;
	Reflector* reflector;
	map<string, map<string, ClassInfo> > classInfoMap;
	friend class ControllerHandler;
	friend class ExtHandler;
	friend class FormHandler;
	friend class SecurityHandler;
	friend class FilterHandler;
	friend class ServiceTask;
public:
	FFEADContext(const string&, const string& appName= "default");
	FFEADContext();
	virtual ~FFEADContext();
	void* getBean(const string&, const string& appName= "default");
	void* getBean(const Bean&);
	void clear(const string& appName= "default");
	void addBean(const Bean& bean);
	void initializeAllSingletonBeans(const map<string, bool>& servingContexts);
	Reflector& getReflector();
	void release(const string& beanName, const string& appName);
};

#endif /* FFEADCONTEXT_H_ */
