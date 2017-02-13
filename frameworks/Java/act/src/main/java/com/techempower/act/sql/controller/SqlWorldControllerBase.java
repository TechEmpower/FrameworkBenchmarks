package com.techempower.act.sql.controller;

import act.db.ebean.EbeanDao;
import act.db.ebean.EbeanQuery;
import com.techempower.act.controller.WorldControllerBase;
import com.techempower.act.sql.domain.World;

public abstract class SqlWorldControllerBase<
		MODEL_TYPE extends World,
		DAO_TYPE extends EbeanDao<Integer, MODEL_TYPE>>
		extends WorldControllerBase<MODEL_TYPE, EbeanQuery<MODEL_TYPE>, DAO_TYPE> {

	public SqlWorldControllerBase(DAO_TYPE worldDao) {
		super(worldDao);
	}

	@Override
	protected MODEL_TYPE findAndModifyOne() {
		MODEL_TYPE model = findOne();
		model.setRandomNumber(randomWorldNumber());
		worldDao.ebean().update(model);
		return model;
	}

}
