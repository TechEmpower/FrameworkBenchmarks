package io.vertx.benchmark.benchmarks;

import io.vertx.core.Vertx;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

public class BaseBenchmark {

    protected static final String SERVER = "vertx-web";
    protected String date;

    public BaseBenchmark(Vertx vertx) {
        date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now());
        // refresh the value as a periodic task
        vertx.setPeriodic(1000, handler -> date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
    }
}
