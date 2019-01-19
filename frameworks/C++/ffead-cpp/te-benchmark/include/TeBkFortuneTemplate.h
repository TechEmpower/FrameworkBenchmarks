/*
 * TeBkFortuneTemplate.h
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#ifndef TEBKFORTUNETEMPLATE_H_
#define TEBKFORTUNETEMPLATE_H_
#include "TemplateHandler.h"
#include "TeBkFortune.h"
#include "vector"
#include "DataSourceManager.h"
#include <stdlib.h>
#include <algorithm>
#include "CryptoHandler.h"

#pragma @Template path="fortunes" file="fortunes.tpe"
class TeBkFortuneTemplate {
public:
	virtual ~TeBkFortuneTemplate();
	void getContext(HttpRequest* request, Context* context);
};

#endif /* TEBKFORTUNETEMPLATE_H_ */
