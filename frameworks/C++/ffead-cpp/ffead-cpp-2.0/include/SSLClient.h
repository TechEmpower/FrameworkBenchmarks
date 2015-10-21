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
 * SSLClient.h
 *
 *  Created on: Dec 14, 2010
 *      Author: sumeet
 */

#ifndef SSLCLIENT_H_
#define SSLCLIENT_H_
#include "SSLCommon.h"
#include "ClientInterface.h"
#include "PropFileReader.h"
using namespace std;

class SSLClient : public ClientInterface {
	SSL *ssl;
	SSL_CTX *ctx;
	BIO *sbio,*io,*ssl_bio;
	SOCKET sockfd;
	static char *pass;
	bool connected, isDHParams;
	string cert_file,key_file,dh_file,ca_list,rand_file,sec_password;
	int client_auth;
	void *get_in_addr1(struct sockaddr *sa);
	void init();
	void destroy_ctx(SSL_CTX *ctx);
	static int password_cb(char *buf, int num, int rwflag, void *userdata);
	Logger logger;
public:
	SSLClient();
	SSLClient(const string& secFile);
	virtual ~SSLClient();
	bool connection(const string&, const int&);
	bool connectionUnresolv(const string& host, const int& port);
	int sendData(string);
	string getData(const string& hdrdelm, const string& cntlnhdr);
	string getData(int cntlen);
	void closeConnection();
	bool isConnected();
	string getBinaryData(const int&, const bool&);
	string getTextData(const string& hdrdelm, const string& cntlnhdr);
};

#endif /* SSLCLIENT_H_ */
