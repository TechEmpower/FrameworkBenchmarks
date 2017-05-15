package com.techempower.act.controller;

import act.controller.Controller;
import act.db.Dao;
import com.techempower.act.domain.IFortune;
import com.techempower.act.mongo.domain.Fortune;
import org.osgl.$;
import org.osgl.mvc.annotation.GetAction;

import java.util.Collections;
import java.util.List;

public abstract class FortuneControllerBase<MODEL_TYPE extends IFortune,
        QUERY_TYPE extends Dao.Query<MODEL_TYPE, QUERY_TYPE>,
        DAO_TYPE extends Dao<Integer, MODEL_TYPE, QUERY_TYPE>> extends Controller.Util  {

    protected DAO_TYPE fortuneDao;

    public FortuneControllerBase(DAO_TYPE fortuneDao) {
        this.fortuneDao = $.notNull(fortuneDao);
    }

    @GetAction("fortunes")
    public void fortunes() {
        List<IFortune> fortunes = (List)fortuneDao.findAllAsList();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        template("fortunes.mustache", fortunes);
    }

}
