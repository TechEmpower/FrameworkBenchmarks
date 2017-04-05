package com.techempower.act.mysql.controller;

import act.controller.Controller;
import com.techempower.act.mysql.domain.World;
import com.techempower.act.sql.controller.SqlWorldControllerBase;

import javax.inject.Inject;

@Controller("mysql")
public class WorldController extends SqlWorldControllerBase<World, World.Dao> {

    private World.Dao worldDao;

    @Inject
    public WorldController(World.Dao worldDao) {
        super(true);
        this.worldDao = worldDao;
    }

    @Override
    protected World.Dao worldDao() {
        return worldDao;
    }
}
