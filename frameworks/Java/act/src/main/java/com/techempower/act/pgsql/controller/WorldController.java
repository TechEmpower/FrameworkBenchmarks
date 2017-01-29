package com.techempower.act.pgsql.controller;

import act.controller.Controller;
import com.techempower.act.pgsql.domain.World;
import com.techempower.act.sql.controller.SqlWorldControllerBase;

import javax.inject.Inject;
import javax.inject.Singleton;

@Controller("pgsql")
@Singleton
public class WorldController extends SqlWorldControllerBase<World, World.Dao> {
    @Inject
    public WorldController(World.Dao worldDao) {
        super(worldDao);
    }
}
