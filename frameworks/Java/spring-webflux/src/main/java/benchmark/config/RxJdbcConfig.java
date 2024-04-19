package benchmark.config;

import org.davidmoten.rx.jdbc.ConnectionProvider;
import org.davidmoten.rx.jdbc.Database;
import org.davidmoten.rx.jdbc.pool.NonBlockingConnectionPool;
import org.davidmoten.rx.jdbc.pool.Pools;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

@Configuration
@Profile("rxjdbc")
public class RxJdbcConfig {
    @Bean
    Database database(DataSourceProperties dsProps) {
        NonBlockingConnectionPool pool =
                Pools.nonBlocking()
                        .maxPoolSize(Runtime.getRuntime().availableProcessors() * 2)
                        .connectionProvider(ConnectionProvider.from(dsProps.getUrl(), dsProps.getUsername(), dsProps.getPassword()))
                        .build();

        return Database.from(pool);
    }
}