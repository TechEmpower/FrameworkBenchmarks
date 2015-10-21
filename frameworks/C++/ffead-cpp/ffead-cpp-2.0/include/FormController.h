/*
 * FormController.h
 *
 *  Created on: Jul 14, 2011
 *      Author: sumeet
 */

#ifndef FORMCONTROLLER_H_
#define FORMCONTROLLER_H_
#include "HttpResponse.h"

class FormController {
public:
	virtual void onSubmit(void*, HttpResponse*)=0;
};

#endif /* FORMCONTROLLER_H_ */
