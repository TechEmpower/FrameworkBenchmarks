package com.techempower.act.mongo.controller;

import act.controller.Controller;
import act.db.morphia.MorphiaQuery;
import com.techempower.act.controller.FortuneControllerBase;
import com.techempower.act.mongo.domain.Fortune;

import javax.inject.Inject;
import javax.inject.Singleton;

@Singleton
@Controller("mongo")
public class FortuneController extends FortuneControllerBase<Fortune, MorphiaQuery<Fortune>, Fortune.Dao> {

    @Inject
    public FortuneController(Fortune.Dao fortuneDao) {
        super(fortuneDao);
    }

}
