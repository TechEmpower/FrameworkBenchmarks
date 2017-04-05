package com.techempower.act.pgsql.controller;

import act.controller.Controller;
import com.techempower.act.pgsql.domain.World;
import com.techempower.act.sql.controller.SqlWorldControllerBase;

import javax.inject.Inject;

@Controller("pgsql")
public class WorldController extends SqlWorldControllerBase<World, World.Dao> {

    private World.Dao worldDao;

    @Inject
    public WorldController(World.Dao worldDao) {
        super(false);
        this.worldDao = worldDao;
    }

    @Override
    protected World.Dao worldDao() {
        return worldDao;
    }
}
