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
 * HttpRequest.h
 *
 *  Created on: Aug 10, 2009
 *      Author: sumeet
 */

#include "map"
#include "HttpSession.h"
#include "vector"
#include "sstream"
#include "fstream"
#include "StringUtil.h"
#include "RegexUtil.h"
#include "CastUtil.h"
#include "stdio.h"
#include <openssl/ssl.h>
#include "CryptoHandler.h"
#include "MultipartContent.h"
#include "Timer.h"
#include "HTTPResponseStatus.h"

typedef vector<string> strVec;
#ifndef HTTPREQUEST_H_
#define HTTPREQUEST_H_
using namespace std;
typedef map<string, string> RMap;
typedef map<string, MultipartContent> FMap;

class HttpRequest {
	static string VALID_REQUEST_HEADERS;
	string webpath;
	string authority;
	string scheme;
	string host;
	string url;
	string ext;
	string cntxt_root;
	string cntxt_home;
	string cntxt_name;
	string httpVersion;
	float httpVers;
	string method;
	string content;
	string content_boundary;
	string content_tfile;
	string file;
	RMap requestParams;
	FMap requestParamsF;
	RMap queryParams;
	HttpSession session;
	strVec localeInfo;
	string actUrl;
	vector<string> actUrlParts;
	string sessionID;
	bool cookie;
	string ranges;
	map<string,string> cookieattrs;
	map<string,string> authinfo;
	map<int,string> reqorderinf;
	map<int,string> authorderinf;
	map<string,string> headers;
	vector<MultipartContent> contentList;
	string preamble;
	string epilogue;
	HTTPResponseStatus status;
	string userName;
	string password;
	string authMethod;

	void getAuthParams(string);
	void getOauthParams(string str);
	void updateFromContentStr_Old();
	void updateFromContentStr();
	void updateFromContentFile();
	void setMethod(const string& method);
	void setRequestParams(const RMap&);
	void setRequestParam(const string&, const string&);
	void setContent_tfile(const string& tfile);
	void setQueryParams(const RMap& queryParams);
	void setAuthinfo(const map<string,string>&);
	void normalizeUrl();
	void setActUrl(const string&);
	void setCntxt_name(const string&);
	void setCntxt_root(const string&);
	void setContent_boundary(const string&);
	void setQueryParam(const string& name, const string& value);
	void setSessionID(const string& sessionID);
	string toPluginString();
	void setHttp2Headers(map<string,string> headers);
	void setContextHome(const string& home);
	friend class ServiceTask;
	friend class ControllerHandler;
	friend class SecurityHandler;
	friend class Http11Handler;
	friend class Http2Handler;
	friend class Http2StreamHandler;
	friend class HttpResponse;
	friend class HttpServiceHandler;
	friend class HttpServiceTask;
public:
	enum {
		PREFLIGHT, CORS, OTHER
	};
	static string Accept,AcceptCharset,AcceptEncoding,AcceptLanguage,AcceptDatetime,
				  AccessControlRequestHeaders,AccessControlRequestMethod,Authorization,
				  CacheControl,Connection,Cookie,ContentLength,ContentMD5,ContentType,
				  Date,Expect,From,Host,IfMatch,IfModifiedSince,IfNoneMatch,IfRange,TransferEncoding,
				  IfUnmodifiedSince,MaxForwards,Origin,Pragma,ProxyAuthorization,Range,
				  Referer,TE,Upgrade,UserAgent,Via,Warning,SecWebSocketKey,SecWebSocketVersion,
				  SecWebSocketAccept,SecWebSocketProtocol,SecWebSocketExtensions,AltUsed,Http2Settings;
	HttpRequest();
	HttpRequest(const strVec&, const string&);
	HttpRequest(const string&);
	void updateContent();
	virtual ~HttpRequest();
    void setUrl(string url);
    HttpSession* getSession();
    string getMethod() const;
	string getUrl() const;
	string getHttpVersion() const;
	float getHttpVers() const;
	string getContent_boundary() const;
	string getContent() const;
	void setContent(const string&);
    RMap getRequestParams() const;
    string getRequestParam(const string&);
    MultipartContent getMultipartContent(const string& key);
    string getRequestParamType(const string& key);
    string getCntxt_root() const;
    string getDefaultLocale() const;
    string getCntxt_name() const;
    string getFile() const;
    void setFile(const string&);
    string getActUrl() const;
    const vector<string>& getActUrlParts() const;
    string getSessionID() const;
    map<string,string> getAuthinfo() const;
    void buildRequestC(const char* key, const char* value);
    void buildRequest(string key, string value);
    string toString();
#ifdef INC_SCRH
    string toPHPVariablesString(const string&);
	string toPerlVariablesString();
	string toRubyVariablesString();
	string toPythonVariablesString();
	string toLuaVariablesString();
	string toNodejsVariablesString();
#endif
	RMap getQueryParams() const;
	string getQueryParam(const string& key);
	RMap getAllParams();
    bool hasCookie();
    map<int,string> getAuthOrderinfo() const;
	map<int,string> getReqOrderinfo() const;
	map<string,string> getCookieInfo() const;
    string getAuthOrderinfoAttribute(const int& key);
    string getReqOrderinfoAttribute(const int& key);
    string getCookieInfoAttribute(const string& key);
    string getHeader(string key);
    bool hasHeader(string key);
    map<string,string> getHeaders();
    int getCORSRequestType();
    void addHeaderValue(string header, const string& value);
    vector<string> parseHeaderValue(string headerValue);
    static bool isValidHttpMethod(const string& method);
    bool isValidHttpMethod();
    bool isAgentAcceptsCE();
    bool isHeaderValue(string header, const string& value, const bool& ignoreCase= true);
    bool hasHeaderValuePart(string header, string valuePart, const bool& ignoreCase= true);
    vector<vector<int> > getRanges(vector<string> &rangesVec);
    string getContent_tfile();
    void addMultipartFormContent(const string& key, const MultipartContent& content);
    void addContent(const MultipartContent& content);
    bool isNonBinary(const string& mimeType);
    string getParamValue(const string&);
    HTTPResponseStatus getRequestParseStatus() const;
    vector<MultipartContent> getMultiPartFileList(const string& name);
	string getPassword() const;
	string getUserName() const;
	string getAuthMethod() const;
	string getContextHome();
	string getExt() const;
	static string getFileExtension(const string& file);
};

#endif /* HTTPREQUEST_H_ */
