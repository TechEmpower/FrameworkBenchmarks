package benchmark.config;

import benchmark.repository.r2dbc.FortuneRepo;
import benchmark.repository.r2dbc.WorldRepo;
import io.r2dbc.postgresql.PostgresqlConnectionConfiguration;
import io.r2dbc.postgresql.PostgresqlConnectionFactory;
import io.r2dbc.spi.ConnectionFactory;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;
import org.springframework.data.r2dbc.function.DatabaseClient;
import org.springframework.data.r2dbc.repository.support.R2dbcRepositoryFactory;
import org.springframework.data.relational.core.mapping.RelationalMappingContext;

@Configuration
@Profile("r2dbc")
@ConfigurationProperties(prefix = "database")
public class R2dbcConfig {
    private String name;
    private String host;
    private int port;
    private String username;
    private String password;

    @Bean
    public PostgresqlConnectionFactory connectionFactory() {
        PostgresqlConnectionConfiguration configuration = PostgresqlConnectionConfiguration
                .builder()
                .host(host)
                .port(port)
                .database(name)
                .username(username)
                .password(password)
                .build();
        return new PostgresqlConnectionFactory(configuration);
    }

    @Bean
    public DatabaseClient databaseClient(ConnectionFactory connectionFactory) {
        return DatabaseClient.create(connectionFactory);
    }

    @Bean
    public R2dbcRepositoryFactory repositoryFactory(DatabaseClient client) {
        RelationalMappingContext context = new RelationalMappingContext();
        context.afterPropertiesSet();

        return new R2dbcRepositoryFactory(client, context);
    }

    @Bean
    public FortuneRepo fortuneRepository(R2dbcRepositoryFactory factory) {
        return factory.getRepository(FortuneRepo.class);
    }

    @Bean
    public WorldRepo worldRepository(R2dbcRepositoryFactory factory) {
        return factory.getRepository(WorldRepo.class);
    }

}