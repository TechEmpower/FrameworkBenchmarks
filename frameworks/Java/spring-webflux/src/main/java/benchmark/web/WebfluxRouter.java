package benchmark.web;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerResponse;

import static org.springframework.web.reactive.function.server.RequestPredicates.GET;

@Configuration
public class WebfluxRouter {

    @Bean
    public RouterFunction<ServerResponse> route(WebfluxHandler handler) {
        return RouterFunctions
                .route(
                        GET("/plaintext"),
                        handler::plaintext)
                .andRoute(
                        GET("/json"),
                        handler::json)
                .andRoute(
                        GET("/db"),
                        handler::db)
                .andRoute(
                        GET("/queries"),
                        handler::queries)
                .andRoute(
                        GET("/updates"),
                        handler::updates)
                .andRoute(
                        GET("/fortunes"),
                        handler::fortunes)
                ;
    }
}