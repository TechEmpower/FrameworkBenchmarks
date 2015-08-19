/*
 * GtmAccesController.h
 *
 *  Created on: 25-Nov-2014
 *      Author: sumeetc
 */

#ifndef GTMACCESCONTROLLER_H_
#define GTMACCESCONTROLLER_H_
#ifdef INC_GTM
#include "GTM.h"
#endif
#include "Controller.h"

class GtmAccesController {
public:
	GtmAccesController();
	virtual ~GtmAccesController();
	bool service(HttpRequest* req, HttpResponse* res);
};

#endif /* GTMACCESCONTROLLER_H_ */
