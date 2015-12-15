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
 * SSLHandler.h
 *
 *  Created on: 20-Jun-2012
 *      Author: sumeetc
 */

#ifndef SSLHANDLER_H_
#define SSLHANDLER_H_
#include "SSLCommon.h"
#include "LoggerFactory.h"
#include "Mutex.h"
#define CLIENT_AUTH_REQUEST 1
#define CLIENT_AUTH_REQUIRE 2
#define CLIENT_AUTH_REHANDSHAKE 3

using namespace std;

class SecurityProperties {
public:
	string cert_file;
	string key_file;
	string dh_file;
	string ca_list;
	string rand_file;
	string sec_password;
	string srv_auth_prvd;
	string srv_auth_mode;
	string srv_auth_file;
	int client_auth;
	long isDHParams;
	bool alpnEnabled;
	vector<string> alpnProtoList;
	friend class SSLHandler;
	friend class ServiceTask;
	friend class ConfigurationHandler;
	friend class CHServer;
	friend class SocketUtil;
};

class SSLHandler {
	Logger logger;
	Mutex lock;
	static char *pass;
	SSL_CTX *ctx;
	bool isSSLEnabled;
	map<int, string> socketAlpnProtoMap;
	static SSLHandler* instance;
	bool isValid;
	SecurityProperties securityProperties;
	SSLHandler();
	void init(const SecurityProperties& securityProperties);
	vector<unsigned char> getDefaultALPN();
	static vector<vector<unsigned char> > advertisedProtos;
	static int next_proto_cb(SSL *s, const unsigned char **data, unsigned int *len, void *arg);
	static int alpn_select_proto_cb(SSL *ssl, const unsigned char **out, unsigned char *outlen, const unsigned char *in, const unsigned int& inlen, void *arg);
	static int select_next_protocol(unsigned char **out, unsigned char *outlen, const unsigned char *in, const unsigned int& inlen);
	static string getAlpnProto(const int& fd);
	static void removeAlpnProtoSocket(const int& fd);
	friend class SocketUtil;
public:
	bool getIsSSL() const;
	SSL_CTX* getCtx() const;
	static int s_server_session_id_context;
	static int s_server_auth_session_id_context;
	static void initInstance(const SecurityProperties& securityProperties);
	static void clear();
	static void setIsSSL(const bool& isSSLEnabled);
	static SSLHandler* getInstance();
	virtual ~SSLHandler();
	static int password_cb(char *buf, int num, int rwflag, void *userdata);
	void closeSSL(const int& fd, SSL *ssl, BIO* bio);
};

#endif /* SSLHANDLER_H_ */
