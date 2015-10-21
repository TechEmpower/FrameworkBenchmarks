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
 * HttpResponse.h
 *
 *  Created on: Aug 19, 2009
 *      Author: sumeet
 */

#ifndef HTTPRESPONSE_H_
#define HTTPRESPONSE_H_
#include "string"
#include "vector"
#include <sys/stat.h>
#include "CastUtil.h"
#include "HttpRequest.h"
#include "HTTPResponseStatus.h"
#include "ContentTypes.h"
#include "Timer.h"
#include "RegexUtil.h"
#include "CompressionUtil.h"
#include "MultipartContent.h"
#include "DateFormat.h"
#include "CommonUtils.h"

using namespace std;
typedef vector<unsigned char> Cont;
class HttpResponse {
public:
	static string AccessControlAllowOrigin,AccessControlAllowHeaders,AccessControlAllowCredentials,
				  AccessControlAllowMethods,AccessControlMaxAge,AcceptRanges,Age,Allow,CacheControl,
				  Connection,ContentEncoding,ContentLanguage,ContentLength,ContentLocation,ContentMD5,
				  ContentDisposition,ContentRange,ContentType,DateHeader,ETag,Expires,LastModified,Link,
				  Location,P3P,Pragma,ProxyAuthenticate,Refresh,RetryAfter,Server,SetCookie,Status,
				  StrictTransportSecurity,Trailer,TransferEncoding,Vary,Via,Warning,WWWAuthenticate,
				  Upgrade,SecWebSocketAccept,SecWebSocketVersion,AltSvc;
	HttpResponse();
	virtual ~HttpResponse();
    string getHttpVersion() const;
    void setHTTPResponseStatus(const HTTPResponseStatus& status);
    string getStatusCode() const;
    void setStatusCode(const string& statusCode);
    string getStatusMsg() const;
    void setStatusMsg(const string& statusMsg);
    string getContent() const;
	//void setContent(const Cont& content);
	void setContent(const string& content);
	void addCookie(const string& cookie);
    void addContent(const MultipartContent& content);
    void addHeaderValue(string header, const string& value);
    bool isHeaderValue(string header, const string& value, const bool& ignoreCase= true);
    bool isNonBinary();
    string getHeader(string);
    bool getCompressed();
	vector<string> getCookies() const;
	string getStatusLine() const;
	string toPluginString();
	bool isDone() const;
	void setDone(const bool& done);

	friend class ServiceTask;
    friend class HttpResponseParser;
    friend class Http11Handler;
    friend class Http2Handler;
    friend class Http2RequestResponseData;
    friend class HttpServiceHandler;
private:
    bool done;
    float httpVers;
    uint32_t intCntLen;
    static string VALID_RESPONSE_HEADERS;
	string httpVersion;
	string statusCode;
	string statusMsg;
	string preamble;
	string epilogue;
	bool compressed;
	map<string, MultipartContent> multipartFormData;
	vector<MultipartContent> contentList;
	string content;
	string outFileName;
	vector<string> cookies;
	map<string,string> headers;
	int techunkSiz;
	int teparts;
	int tecurrpart;
	bool hasContent;
	void setCompressed(const bool& compressed);
	void update(HttpRequest* req);
	string generateResponse(const string& httpMethod, HttpRequest *req, const bool& appendHeaders= true);
	string generateResponse(const bool& appendHeaders= true);
	string generateOnlyHeaderResponse(HttpRequest *req);
	string generateHeadResponse();
	string generateOptionsResponse();
	string generateTraceResponse(HttpRequest* req);
	bool updateContent(HttpRequest* req, const uint32_t& techunkSiz);
	unsigned int getContentSize(const char *fileName);
	string getContent(const char *fileName, const int& start= -1, const int& end= -1);
	bool isContentRemains();
	string getRemainingContent(const string& fname, const bool& isFirst);
	static string getFileExtension(const string& file);
};

#endif /* HTTPRESPONSE_H_ */
