package benchmark.web;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerResponse;

@Configuration
public class WebfluxRouter {

    @Bean
    public RouterFunction<ServerResponse> route(WebfluxHandler handler) {
        return RouterFunctions.route()
                .GET("/plaintext", handler::plaintext)
                .GET("/json", handler::json)
                .GET("/db", handler::db)
                .GET("/queries", handler::queries)
                .GET("/updates", handler::updates)
                .GET("/fortunes", handler::fortunes)
                .build();
    }
}