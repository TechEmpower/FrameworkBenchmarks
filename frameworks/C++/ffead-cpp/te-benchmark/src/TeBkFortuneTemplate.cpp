/*
 * TeBkFortuneTemplate.cpp
 *
 *  Created on: 11-Mar-2015
 *      Author: sumeetc
 */

#include "TeBkFortuneTemplate.h"

TeBkFortuneTemplate::~TeBkFortuneTemplate() {
	// TODO Auto-generated destructor stub
}

void TeBkFortuneTemplate::getContext(HttpRequest* request, Context* context)
{
	DataSourceInterface* sqli = DataSourceManager::getImpl();
	std::vector<TeBkFortune> flst = sqli->getAll<TeBkFortune>();
	for(int i=0;i<(int)flst.size();i++)
	{
		std::string nm = flst.at(i).getMessage();
		CryptoHandler::sanitizeHtml(nm);
		flst.at(i).setMessage(nm);
	}

	TeBkFortune nf;
	nf.setId(0);
	nf.setMessage("Additional fortune added at request time.");
	flst.push_back(nf);
	std::sort (flst.begin(), flst.end());
	delete sqli;

	GenericObject& fortunes = (*context)["fortunes"];
	fortunes << flst;
}
