package com.techempower.act.mysql.controller;

import act.controller.Controller;
import com.techempower.act.mysql.domain.Fortune;

import javax.inject.Inject;
import javax.inject.Singleton;

@Controller("mysql")
@Singleton
public class FortuneController extends com.techempower.act.controller.FortuneControllerBase<Fortune,act.db.ebean.EbeanQuery<Fortune>,Fortune.Dao> {
    @Inject
    public FortuneController(Fortune.Dao worldDao) {
        super(worldDao);
    }
}
