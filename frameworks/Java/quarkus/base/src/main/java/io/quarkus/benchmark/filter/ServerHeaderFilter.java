package io.quarkus.benchmark.filter;

import java.util.Timer;
import java.util.TimerTask;

import javax.ws.rs.container.ContainerRequestContext;
import javax.ws.rs.container.ContainerResponseContext;
import javax.ws.rs.container.ContainerResponseFilter;
import javax.ws.rs.ext.Provider;

import org.jboss.resteasy.util.HttpHeaderNames;

@Provider
public class ServerHeaderFilter implements ContainerResponseFilter {

    private final DateHelper dateHelper = new DateHelper();

    @Override
    public void filter(ContainerRequestContext requestContext, ContainerResponseContext responseContext) {
        responseContext.getHeaders().add("Server", "Quarkus");
        responseContext.getHeaders().add(HttpHeaderNames.DATE, dateHelper.getDate());
    }

    static class DateHelper extends TimerTask {
        private Timer timer = new Timer();
        private String date = generateDate();

        public DateHelper() {
            timer.schedule(this, 0, 1000);
        }

        @Override
        public void run() {
            date = generateDate();
        }

        private String generateDate() {
            return java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now());
        }

        public String getDate() {
            return date;
        }
    }
}