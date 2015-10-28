/*
 * IDGenerator.h
 *
 *  Created on: 10-Oct-2014
 *      Author: sumeetc
 */

#ifndef IDGENERATOR_H_
#define IDGENERATOR_H_
#include "Compatibility.h"
#include "DataSourceMapping.h"
#include "GenericObject.h"
#include "HiloIdGenerator.h"
#ifdef HAVE_LIBUUID
#include <uuid/uuid.h>
#endif

class IDGenerator {
public:
	void init(DataSourceEntityMapping& dsemp, const bool& forceReinit= false);
	virtual void executePreTable(DataSourceEntityMapping& dsemp, GenericObject&)=0;
	virtual void executePostTable(DataSourceEntityMapping& dsemp, GenericObject& id)=0;
	virtual void executeSequence(DataSourceEntityMapping& dsemp, GenericObject&)=0;
	virtual void executeIdentity(DataSourceEntityMapping& dsemp, GenericObject&)=0;
	virtual void executeCustom(DataSourceEntityMapping& dsemp, const string& customMethod, GenericObject&)=0;
	void next(DataSourceEntityMapping& dsemp, GenericObject&);
	IDGenerator();
	virtual ~IDGenerator();
};

#endif /* IDGENERATOR_H_ */
