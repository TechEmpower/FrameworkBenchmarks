package io.quarkus.benchmark.filter;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import javax.inject.Singleton;

import io.quarkus.scheduler.Scheduled;
import io.quarkus.vertx.web.RouteFilter;
import io.vertx.ext.web.RoutingContext;

@Singleton
public class ServerHeaderFilter {

    private String date;

    @Scheduled(every="1s")
    void increment() {
        date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now());
    }

    @RouteFilter(100) 
    void myFilter(RoutingContext rc) {
       rc.response().putHeader( "Server", "Quarkus");
       rc.response().putHeader( "Date", date);
       rc.next(); 
    }
}