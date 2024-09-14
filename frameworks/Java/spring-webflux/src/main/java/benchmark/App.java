package benchmark;

import benchmark.web.ServerFilter;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.context.annotation.Bean;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.reactive.function.server.HandlerStrategies;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import org.springframework.web.reactive.function.server.ServerResponse;
import org.springframework.web.reactive.result.view.ViewResolver;
import org.springframework.web.server.WebHandler;
import org.springframework.web.server.adapter.WebHttpHandlerBuilder;

@SpringBootApplication
@EnableScheduling
public class App  {

    @Bean
    public HttpHandler httpHandler(RouterFunction<ServerResponse> route, ServerFilter serverFilter, ViewResolver viewResolver) {
        WebHandler webHandler = RouterFunctions.toWebHandler(route, HandlerStrategies.builder().viewResolver(viewResolver).build());
        return WebHttpHandlerBuilder.webHandler(webHandler).filter(serverFilter).build();
    }

    public static void main(String[] args) {
        SpringApplication.run(App.class, args);
    }

}