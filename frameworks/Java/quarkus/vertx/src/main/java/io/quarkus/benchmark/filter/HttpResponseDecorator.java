package io.quarkus.benchmark.filter;

import io.vertx.core.Vertx;
import io.vertx.core.http.HttpHeaders;
import io.vertx.core.http.HttpServerResponse;
import jakarta.annotation.PreDestroy;
import jakarta.inject.Singleton;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

@Singleton
public class HttpResponseDecorator {
    private static final CharSequence HEADER_DATE = HttpHeaders.createOptimized("date");
    private static final CharSequence QUARKUS_SERVER = HttpHeaders.createOptimized("quarkus");
    private final Vertx vertx;
    private CharSequence date;
    private final long timerId;

    public HttpResponseDecorator(final Vertx vertx) {
        this.vertx = vertx;
        date = createDateHeader();
        timerId = vertx.setPeriodic(1000, ignore -> date = createDateHeader());
    }

    public static CharSequence createDateHeader() {
        return HttpHeaders.createOptimized(DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now()));
    }

    @PreDestroy
    public void destroy() {
        vertx.cancelTimer(timerId);
    }

    public void decorate(final HttpServerResponse response) {
        final var headers = response.headers();
        headers.add(HttpHeaders.SERVER, QUARKUS_SERVER);
        headers.add(HEADER_DATE, date);
    }
}
