package benchmark.web;

import reactor.core.publisher.Mono;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.web.reactive.function.server.HandlerFunction;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.ServerResponse;

@Configuration
public class WebfluxRouter {

    @Bean
    public RouterFunction<ServerResponse> route(
            TextHandler textHandler, JsonHandler jsonHandler, DbHandler dbHandler) {

        return request -> {
            HandlerFunction<ServerResponse> fn = switch (request.uri().getRawPath()) {
                case "/plaintext" -> textHandler;
                case "/json" -> jsonHandler;
                case "/db" -> dbHandler::db;
                case "/queries" -> dbHandler::queries;
                case "/updates" -> dbHandler::updates;
                case "/fortunes" -> dbHandler::fortunes;
                default -> r -> ServerResponse.notFound().build();
            };
            return Mono.just(fn);
        };
    }

}