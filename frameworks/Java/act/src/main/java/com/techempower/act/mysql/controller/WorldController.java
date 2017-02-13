package com.techempower.act.mysql.controller;

import act.controller.Controller;
import com.techempower.act.mysql.domain.World;
import com.techempower.act.sql.controller.SqlWorldControllerBase;

import javax.inject.Inject;
import javax.inject.Singleton;

@Controller("mysql")
@Singleton
public class WorldController extends SqlWorldControllerBase<World, World.Dao> {
    @Inject
    public WorldController(World.Dao worldDao) {
        super(worldDao);
    }
}
