package hello;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;

@Profile("jpa")
@Configuration
@EnableJpaRepositories(basePackages = "hello.jpa")
public class JpaConfig {

}
