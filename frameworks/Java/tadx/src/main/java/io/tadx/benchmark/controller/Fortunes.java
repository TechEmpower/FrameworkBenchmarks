package io.tadx.benchmark.controller;

import io.tadx.benchmark.entity.Fortune;
import io.tadx.data.DbStorage;
import io.tadx.web.annotation.*;
import io.tadx.web.*;
import io.tadx.web.template.FreemarkerEngine;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.SplittableRandom;

/**
 * EN: The entry point of the application.
 */

@RestController
public class Fortunes {


    private static final SplittableRandom RANDOM = new SplittableRandom();
    private final DbStorage dbStorage;

    public Fortunes(DbStorage dbStorage) {
        this.dbStorage = dbStorage;
    }

    @RestFunction(mapping = "/fortunes_2", method = HttpMethod.GET)
    public WebResult<?> execute() {
        List<Fortune> fortunes = dbStorage.queryEntityList(Fortune.class);
        fortunes.addFirst(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        return WebResult.pageResult(FreemarkerEngine.class, "/templates/Fortunes_freemarker.htm");
        //return WebResult.pageResult(DataMap.createNew().put("fortunes", fortunes), "/templates/Fortunes_thymeleaf.htm", TemplateType.THYMELEAF);
    }

}
