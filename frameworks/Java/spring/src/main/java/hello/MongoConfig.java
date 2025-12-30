package hello;

import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.mongodb.repository.config.EnableMongoRepositories;

@Profile("mongo")
@Configuration
@EnableMongoRepositories(basePackages = "hello.repository")
public class MongoConfig {

}
