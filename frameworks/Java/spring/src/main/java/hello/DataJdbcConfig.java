package hello;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.jdbc.repository.config.EnableJdbcRepositories;

@Profile("data-jdbc")
@Configuration
@EnableJdbcRepositories(basePackages = "hello.repository")
public class DataJdbcConfig {

}
