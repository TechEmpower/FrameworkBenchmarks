/*
 * TemplateGenerator.h
 *
 *  Created on: 15-Feb-2013
 *      Author: sumeetc
 */

#ifndef TEMPLATEGENERATOR_H_
#define TEMPLATEGENERATOR_H_
#include "string"
#include "vector"
#include "fstream"
#include "StringUtil.h"
#include "map"
#include "ConfigurationData.h"
using namespace std;


class TemplateGenerator {
public:
	TemplateGenerator();
	virtual ~TemplateGenerator();
	static string generateTempCd(const string&, string &, string &, const string& app);
	static string generateTempCdAll(const string& serverRootDirectory);
};

#endif /* TEMPLATEGENERATOR_H_ */
