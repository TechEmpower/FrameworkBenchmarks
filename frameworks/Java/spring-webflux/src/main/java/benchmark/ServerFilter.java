package benchmark;

import io.netty.handler.codec.http.HttpHeaderNames;
import org.springframework.http.HttpHeaders;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

public class ServerFilter implements WebFilter {
    private static final String SERVER_NAME = "spring-webflux";

    @Override
    public Mono<Void> filter(ServerWebExchange exchange, WebFilterChain chain) {
        HttpHeaders headers = exchange.getResponse().getHeaders();
        headers.add(HttpHeaderNames.SERVER.toString(), SERVER_NAME);
        headers.add(HttpHeaderNames.DATE.toString(), java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now()));
        return chain.filter(exchange);
    }
}
