package io.quarkus.benchmark.filter;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import javax.inject.Singleton;

import io.quarkus.scheduler.Scheduled;
import io.quarkus.vertx.web.RouteFilter;
import io.vertx.core.http.HttpHeaders;
import io.vertx.ext.web.RoutingContext;

@Singleton
public class ServerHeaderFilter {

    private static final CharSequence SERVER_HEADER_NAME = HttpHeaders.createOptimized("Server");
    private static final CharSequence SERVER_HEADER_VALUE = HttpHeaders.createOptimized("Quarkus");
    private static final CharSequence DATE_HEADER_NAME = HttpHeaders.createOptimized("Date");

    private CharSequence date;

    @Scheduled(every="1s")
    void increment() {
        date = HttpHeaders.createOptimized(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
    }

    @RouteFilter(100) 
    void myFilter(RoutingContext rc) {
       rc.response().putHeader( SERVER_HEADER_NAME, SERVER_HEADER_VALUE);
       rc.response().putHeader( DATE_HEADER_NAME, date);
       rc.next(); 
    }
}