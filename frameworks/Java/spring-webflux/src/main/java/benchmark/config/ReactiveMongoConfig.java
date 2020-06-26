package benchmark.config;

import com.mongodb.reactivestreams.client.MongoClient;
import com.mongodb.reactivestreams.client.MongoClients;
import org.slf4j.LoggerFactory;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.mongodb.config.AbstractReactiveMongoConfiguration;
import org.springframework.data.mongodb.core.ReactiveMongoTemplate;
import org.springframework.data.mongodb.repository.config.EnableReactiveMongoRepositories;

@Configuration
@EnableReactiveMongoRepositories
@Profile("mongo")
@ConfigurationProperties(prefix = "database")
public class ReactiveMongoConfig extends AbstractReactiveMongoConfiguration {
    private String url;
    private String name;

    @Bean
    public MongoClient reactiveMongoClient() {
        LoggerFactory.getLogger(getClass()).info("Connecting to mongo url: {}/{}", url, name);
        return MongoClients.create(url);
    }

    @Override
    protected String getDatabaseName() {
        return name;
    }

    @Bean
    public ReactiveMongoTemplate reactiveMongoTemplate() {
        return new ReactiveMongoTemplate(reactiveMongoClient(), getDatabaseName());
    }

    public String getUrl() {
        return url;
    }

    public void setUrl(String url) {
        this.url = url;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }
}
