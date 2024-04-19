package benchmark;

import java.util.concurrent.Executors;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.context.properties.EnableConfigurationProperties;
import org.springframework.boot.web.reactive.result.view.MustacheViewResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.reactive.config.EnableWebFlux;
import org.springframework.web.reactive.config.ViewResolverRegistry;
import org.springframework.web.reactive.config.WebFluxConfigurer;

import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

@SpringBootApplication
@EnableWebFlux
@EnableScheduling
@EnableConfigurationProperties
public class App implements WebFluxConfigurer {

    @Autowired
    private MustacheViewResolver mustacheViewResolver;

    public static void main(String[] args) {
        SpringApplication.run(App.class, args);
    }

    @Bean
    ServerFilter serverFilter() {
        return new ServerFilter();
    }

    @Bean
    DateHandler dateHandler() {
        return new DateHandler();
    }

    @Bean
    Scheduler ioScheduler() {
        return Schedulers.fromExecutor(Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() * 2));
    }

    @Override
    public void configureViewResolvers(ViewResolverRegistry registry) {
        registry.viewResolver(mustacheViewResolver);
    }

}