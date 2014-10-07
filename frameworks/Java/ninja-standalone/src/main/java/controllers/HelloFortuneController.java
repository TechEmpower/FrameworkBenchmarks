package controllers;

import dao.FortuneDao;
import model.Fortune;

import java.util.Collections;
import java.util.List;

import ninja.Result;
import ninja.Results;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import ninja.jpa.UnitOfWork;

@Singleton
public class HelloFortuneController {

    @Inject
    FortuneDao fortuneDao;

    @UnitOfWork
    public Result index() {
        List<Fortune> fortunes = fortuneDao.getAll();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);

        return Results.html().render("fortunes", fortunes);
    }
}
