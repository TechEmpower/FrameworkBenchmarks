package benchmark.web;

import org.springframework.http.HttpHeaders;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;
import org.springframework.web.server.ServerWebExchange;
import org.springframework.web.server.WebFilter;
import org.springframework.web.server.WebFilterChain;
import reactor.core.publisher.Mono;

@Component
public class ServerFilter implements WebFilter {

    private static final String SERVER_NAME = "spring-webflux";

    private String date;

    public ServerFilter() {
        updateDate();
    }

    @Scheduled(fixedRate = 1000)
    public void updateDate() {
        this.date = java.time.format.DateTimeFormatter.RFC_1123_DATE_TIME.format(java.time.ZonedDateTime.now());
    }

    @Override
    public Mono<Void> filter(ServerWebExchange exchange, WebFilterChain chain) {
        HttpHeaders headers = exchange.getResponse().getHeaders();
        headers.add(HttpHeaders.SERVER, SERVER_NAME);
        headers.add(HttpHeaders.DATE, this.date);
        return chain.filter(exchange);
    }
}