/*
 * CommonUtils.h
 *
 *  Created on: 10-Dec-2014
 *      Author: sumeetc
 */

#ifndef COMMONUTILS_H_
#define COMMONUTILS_H_
#include "vector"
#include "string"
#include "map"
#include "iostream"
#include "bitset"
#include "cstring"
#include <stdio.h>
#include <stdint.h>
#include "ThreadLocal.h"
#include "PropFileReader.h"
#include "RegexUtil.h"
using namespace std;

class CommonUtils {
	static string BLANK;
	static ThreadLocal contextName;
	static map<string, string> mimeTypes;
	static map<string, string> locales;
public:
	static unsigned long long charArrayToULongLong(const string& l, int ind);
	static unsigned long long charArrayToULongLong(const string& l);
	static string ulonglongTocharArray(const unsigned long long& lon, const int& provind= -1);
	static unsigned long long charArrayToULongLong(const vector<unsigned char>& l);
	static string xorEncryptDecrypt(const string& toEncrypt, const uint32_t& maskingKey);
	static void printBinary(const string& encv, const bool& isNL= true);
	static string toBinary(const string& encv);
	static void printMap(const map<string, string>& mp);
	static void printMap(const map<int, map<string, string> >& mp);
	static void printMap(const map<string, int>& mp);
	static void printHEX(const string& encv);
	static string toHEX(const uint32_t& num);
	static void setAppName(const string& appName);
	static string getAppName(const string& appName = "");
	static void loadMimeTypes(const string& file);
	static void loadLocales(const string& file);
	static const string& getMimeType(const string& extension);
	static const string& getLocale(const string& abbrev);
};

#endif /* COMMONUTILS_H_ */
