package io.quarkus.benchmark.filter;

import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

import javax.inject.Singleton;
import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.ext.Provider;

import io.quarkus.scheduler.Scheduled;

@Singleton
@Provider
public class ServerHeaderFilter implements ContainerResponseFilter {

    private String date;

    @Scheduled(every="1s")
    void increment() {
        date = DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now());
    }

    @Override
    public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext) {
        final MultivaluedMap<String, Object> headers = responseContext.getHeaders();
        headers.add( "Server", "Quarkus");
        headers.add( "Date", date);
    }
}