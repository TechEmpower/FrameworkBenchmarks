/*
	Copyright 2009-2012, Sumeet Chhetri 
  
    Licensed under the Apache License, Version 2.0 (the "License"); 
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
 * ConnectionSettings.h
 *
 *  Created on: Jan 30, 2010
 *      Author: sumeet
 */

#ifndef CONNECTIONSETTINGS_H_
#define CONNECTIONSETTINGS_H_
#include "string"
using namespace std;

class ConnectionSettings {
	string mode;//ldap|database|filesystem
	string source;
	string username;
	string passwd;
	string address;
public:
	ConnectionSettings();
	ConnectionSettings(string,string,string,string,string);
	virtual ~ConnectionSettings();
	friend class ComponentGen;
};

#endif /* CONNECTIONSETTINGS_H_ */
/*
mode=ldap
source=host:port
username=user
passwd=user
address=ldap://example.com:9765

mode=database
source=dsn-name
username=user
passwd=user
address=oracle://host:port

mode=filesystem
source=host:port
username=user //ftp
passwd=user //ftp
address=ftp://example.com:9765
*/
