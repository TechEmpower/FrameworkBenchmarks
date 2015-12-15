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
 * CryptoHandler.h
 *
 *  Created on: Nov 24, 2010
 *      Author: sumeet
 */

#ifndef CRYPTOHANDLER_H_
#define CRYPTOHANDLER_H_
#include "AppDefines.h"
#include "Compatibility.h"  // WinSock subsystem
#include <openssl/engine.h>
#include <openssl/hmac.h>
#include <openssl/sha.h>
#include <openssl/evp.h>
#include <stdio.h>
#include "string"
#include "cstring"
#include <stdlib.h>
#include "StringUtil.h"
using namespace std;

class CryptoHandler {
public:
	static string base64encodeStr(const string& input);
	static string base64decodeStr(const string& input);
	static char* base64encode(const unsigned char *input, const int& length);
	static char* base64decode(unsigned char *input, const int& length);
	static char* hmac_sha1(char*, char*, const bool&);
	static char* hmac_sha256(char*, char*, const bool&);
	static char* hmac_sha384(char*, char*, const bool&);
	static char* hmac_sha512(char*, char*, const bool&);
	static string sha1(const string&);
	static string urlEncode(const string& str);
	static string urlDecode(const string& str);
	static void sanitizeHtml(string& data);
};

#endif /* CRYPTOHANDLER_H_ */
