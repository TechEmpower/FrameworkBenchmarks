package com.techempower.act.mysql.controller;

import act.controller.Controller;
import com.techempower.act.domain.IFortune;
import com.techempower.act.mysql.domain.Fortune;
import org.osgl.mvc.annotation.GetAction;

import java.util.Collections;
import java.util.List;

import static act.controller.Controller.Util.template;

@Controller("mysql")
public class FortuneController  {

    @GetAction("fortunes")
    public void fortunes(Fortune.Dao fortuneDao) {
        List<IFortune> fortunes = (List)fortuneDao.all();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        template("fortunes", fortunes);
    }

}
