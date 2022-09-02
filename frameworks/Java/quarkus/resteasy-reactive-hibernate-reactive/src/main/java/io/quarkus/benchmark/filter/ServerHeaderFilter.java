package io.quarkus.benchmark.filter;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import javax.annotation.PostConstruct;
import javax.inject.Singleton;

import io.vertx.core.MultiMap;
import org.jboss.resteasy.reactive.server.ServerResponseFilter;

import io.quarkus.scheduler.Scheduled;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServerResponse;

@Singleton
public class ServerHeaderFilter {

    private static final CharSequence SERVER_HEADER_NAME = HttpHeaders.createOptimized("Server");
    private static final CharSequence SERVER_HEADER_VALUE = HttpHeaders.createOptimized("Quarkus");
    private static final CharSequence DATE_HEADER_NAME = HttpHeaders.createOptimized("Date");
    
    private volatile CharSequence date;

    @PostConstruct
    public void init() {
        date = HttpHeaders.createOptimized(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
    }

    @Scheduled(every="1s")
    void increment() {
        date = HttpHeaders.createOptimized(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
    }

    @ServerResponseFilter
    public void filter(HttpServerResponse response) {
        MultiMap headers = response.headers();
        headers.add(SERVER_HEADER_NAME, SERVER_HEADER_VALUE);
        headers.add(DATE_HEADER_NAME, date);
    }
}