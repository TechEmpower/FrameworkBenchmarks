package io.quarkus.benchmark.filter;

import io.quarkus.scheduler.Scheduled;
import io.quarkus.vertx.web.RouteFilter;
import io.vertx.core.http.HttpHeaders;
import io.vertx.ext.web.RoutingContext;
import jakarta.annotation.PostConstruct;
import jakarta.inject.Singleton;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

@Singleton
public class ServerHeaderFilter {

    private static final CharSequence SERVER_HEADER_VALUE = HttpHeaders.createOptimized("Quarkus");
    private CharSequence date;

    @PostConstruct
    public void init() {
        updateDate();
    }

    @Scheduled(every = "1s")
    void updateDate() {
        date = HttpHeaders.createOptimized(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
    }

    @RouteFilter(100)
    void addDefaultHeaders(final RoutingContext rc) {
        final var headers = rc.response().headers();
        headers.add(HttpHeaders.SERVER, SERVER_HEADER_VALUE);
        headers.add(HttpHeaders.DATE, date);
        rc.next();
    }
}