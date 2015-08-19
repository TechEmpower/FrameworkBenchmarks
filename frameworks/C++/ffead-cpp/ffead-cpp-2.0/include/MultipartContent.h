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
 * MultipartContent.h
 *
 *  Created on: 28-Apr-2013
 *      Author: sumeetc
 */

#ifndef MULTIPARTCONTENT_H_
#define MULTIPARTCONTENT_H_
#include "map"
#include "string"
#include "CryptoHandler.h"
#include "RegexUtil.h"
#include <fstream>
#include "ContentTypes.h"
using namespace std;

class MultipartContent {
	static string VALID_HEADERS;
	map<string,string> headers;
	string content;
	string tempFileName;
	string fileName;
	string name;
	void setContent(const string& content);
	void setName(const string& name);
	void setTempFileName(const string& tempFileName);
	string getTempFileName() const;
	void setFileName(const string& fileName);
	friend class HttpResponse;
	friend class HttpRequest;
	friend class ControllerHandler;
public:
	static string ContentId,ContentDisposition,ContentTransferEncoding,
				  ContentLocation,ContentBase,ContentLength,ContentMD5,ContentType;
	MultipartContent();
	MultipartContent(const string& content);
	MultipartContent(const vector<string>& headers, const string& content= "");
	virtual ~MultipartContent();
	void addHeaderValue(string header, const string& value);
	string getContent() const;
	string getFileName() const;
	map<string, string> getHeaders();
	bool isHeaderValue(string header, const string& value, const bool& ignoreCase= true);
	string getHeader(const string&);
	string getName() const;
	bool isAFile();
	bool isValid();
};

#endif /* MULTIPARTCONTENT_H_ */
