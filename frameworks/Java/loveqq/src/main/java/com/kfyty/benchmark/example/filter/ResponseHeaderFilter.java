package com.kfyty.benchmark.example.filter;

import com.kfyty.loveqq.framework.core.autoconfig.annotation.Component;
import com.kfyty.loveqq.framework.web.core.filter.Filter;
import com.kfyty.loveqq.framework.web.core.filter.FilterChain;
import com.kfyty.loveqq.framework.web.core.http.ServerRequest;
import com.kfyty.loveqq.framework.web.core.http.ServerResponse;
import org.reactivestreams.Publisher;
import reactor.core.publisher.Mono;

import java.time.Clock;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;

@Component
public class ResponseHeaderFilter implements Filter {
    private static final Clock clock = Clock.systemDefaultZone();

    @Override
    public Publisher<Void> doFilter(ServerRequest request, ServerResponse response, FilterChain chain) {
        response.setHeader("Server", "loveqq");
        response.setHeader("Date", DateTimeFormatter.RFC_1123_DATE_TIME.format(ZonedDateTime.now(clock)));
        return Mono.from(chain.doFilter(request, response));
    }
}
