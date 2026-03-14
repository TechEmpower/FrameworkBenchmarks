package hello;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.boot.autoconfigure.data.jdbc.JdbcRepositoriesAutoConfiguration;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Profile("jooq")
@Configuration
@EnableAutoConfiguration(exclude = { JdbcRepositoriesAutoConfiguration.class })
public class JooqConfig {

}
