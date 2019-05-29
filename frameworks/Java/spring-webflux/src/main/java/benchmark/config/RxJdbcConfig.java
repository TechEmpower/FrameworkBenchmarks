package benchmark.config;

import org.davidmoten.rx.jdbc.ConnectionProvider;
import org.davidmoten.rx.jdbc.Database;
import org.davidmoten.rx.jdbc.pool.NonBlockingConnectionPool;
import org.davidmoten.rx.jdbc.pool.Pools;
import org.springframework.boot.autoconfigure.jdbc.DataSourceProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

import java.sql.SQLException;

@Configuration
@Profile("rxjdbc")
public class RxJdbcConfig {
    @Bean
    public Database database(DataSourceProperties dsProps) throws SQLException {
        NonBlockingConnectionPool pool =
                Pools.nonBlocking()
                        .maxPoolSize(Runtime.getRuntime().availableProcessors() * 2)
                        .connectionProvider(ConnectionProvider.from(dsProps.getUrl(), dsProps.getUsername(), dsProps.getPassword()))
                        .build();

        Database db = Database.from(pool);

        return db;
    }
}