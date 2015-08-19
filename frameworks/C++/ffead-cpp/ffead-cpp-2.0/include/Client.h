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
 * Client.h
 *
 *  Created on: Mar 27, 2010
 *      Author: sumeet
 */

#ifndef CLIENT_H_
#define CLIENT_H_
#include "AppDefines.h"
#include "ClientInterface.h"
using namespace std;
class Client : public ClientInterface {
	bool connected;
	SOCKET sockfd;
	Logger logger;
public:
	Client();
	virtual ~Client();
	bool connection(const string&, const int&);
	bool connectionUnresolv(const string& host, const int& port);
	int sendData(string);
	string getTextData(const string& hdrdelm, const string& cntlnhdr);
	string getBinaryData(const int&, const bool&);
	void closeConnection();
	bool isConnected();
	void setSocketBlocking();
	void setSocketNonBlocking();
	int receive(string& buf, const int& flag);
	int receivelen(string& buf, const int& len, const int& flag);
	string getData();
	int sendlen(const string& buf, const int& len);
};

#endif /* CLIENT_H_ */
