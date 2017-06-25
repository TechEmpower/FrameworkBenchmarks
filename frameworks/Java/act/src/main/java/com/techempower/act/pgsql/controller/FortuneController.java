package com.techempower.act.pgsql.controller;

import act.controller.Controller;
import com.techempower.act.domain.IFortune;
import com.techempower.act.pgsql.domain.Fortune;
import org.osgl.mvc.annotation.GetAction;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.Collections;
import java.util.List;

@Controller("pgsql")
@Singleton
public class FortuneController extends com.techempower.act.controller.FortuneControllerBase<Fortune,act.db.ebean2.EbeanQuery<Fortune>,Fortune.Dao> {
    @Inject
    public FortuneController(Fortune.Dao worldDao) {
        super(worldDao);
    }

    @GetAction("forythm/")
    public void fortunesWithRythm() {
        List<IFortune> fortunes = (List)fortuneDao.findAllAsList();
        fortunes.add(new com.techempower.act.mongo.domain.Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        renderTemplate("fortunes.html", fortunes);
    }

}
