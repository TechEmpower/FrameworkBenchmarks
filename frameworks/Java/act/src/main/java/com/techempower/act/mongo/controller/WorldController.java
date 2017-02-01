package com.techempower.act.mongo.controller;

import act.controller.Controller;
import act.db.morphia.MorphiaQuery;
import com.techempower.act.controller.WorldControllerBase;
import com.techempower.act.mongo.domain.World;

import javax.inject.Inject;
import javax.inject.Singleton;

@Singleton
@Controller("mongo")
public final class WorldController extends WorldControllerBase<World, MorphiaQuery<World>, World.Dao> {

	@Inject
	public WorldController(World.Dao worldDao) {
		super(worldDao);
	}

	@Override
	protected World findAndModifyOne() {
		return worldDao.ds().findAndModify(worldDao.q("_id", randomWorldNumber()), worldDao.updates().set("randomNumber", randomWorldNumber()));
	}
}
