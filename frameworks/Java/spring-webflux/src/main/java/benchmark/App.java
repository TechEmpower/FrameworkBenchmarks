package benchmark;

import com.samskivert.mustache.Mustache;
import com.zaxxer.hikari.HikariDataSource;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.boot.autoconfigure.jdbc.DataSourceAutoConfiguration;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.boot.autoconfigure.mustache.MustacheResourceTemplateLoader;
import org.springframework.boot.web.reactive.result.view.MustacheViewResolver;
import org.springframework.context.annotation.Bean;
import org.springframework.data.mongodb.repository.config.EnableReactiveMongoRepositories;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.web.reactive.config.EnableWebFlux;
import org.springframework.web.reactive.config.ViewResolverRegistry;
import org.springframework.web.reactive.config.WebFluxConfigurer;
import org.springframework.web.reactive.result.view.ViewResolver;
import reactor.core.scheduler.Scheduler;
import reactor.core.scheduler.Schedulers;

import javax.sql.DataSource;
import java.util.concurrent.Executors;

@SpringBootApplication(exclude = DataSourceAutoConfiguration.class)
@EnableWebFlux
@EnableScheduling
public class App implements WebFluxConfigurer {

    public static void main(String[] args) {
        SpringApplication.run(App.class, args);
    }

    @Bean
    public ServerFilter serverFilter() {
        return new ServerFilter();
    }

    @Bean
    public DateHandler dateHandler() {
        return new DateHandler();
    }

    @Bean
    public Scheduler ioScheduler() {
        return Schedulers.fromExecutor(Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors() * 2));
    }

    @Bean
    public ViewResolver mustacheViewResolver() {
        String prefix = "classpath:/templates/";
        String suffix = ".mustache";
        Mustache.TemplateLoader loader = new MustacheResourceTemplateLoader(prefix, suffix);
        MustacheViewResolver mustacheViewResolver = new MustacheViewResolver(Mustache.compiler().withLoader(loader));
        mustacheViewResolver.setPrefix(prefix);
        mustacheViewResolver.setSuffix(suffix);
        return mustacheViewResolver;
    }

    @Override
    public void configureViewResolvers(ViewResolverRegistry registry) {
        registry.viewResolver(mustacheViewResolver());
    }

    @Bean
    public DataSource datasource(DataSourceProperties dataSourceProperties) {
        HikariDataSource dataSource = (HikariDataSource) dataSourceProperties.initializeDataSourceBuilder().type(HikariDataSource.class).build();
        dataSource.setMaximumPoolSize(Runtime.getRuntime().availableProcessors() * 2);

        return dataSource;
    }
}
