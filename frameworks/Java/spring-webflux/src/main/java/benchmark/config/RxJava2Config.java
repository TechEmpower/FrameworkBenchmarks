package benchmark.config;

import org.davidmoten.rx.jdbc.ConnectionProvider;
import org.davidmoten.rx.jdbc.Database;
import org.davidmoten.rx.jdbc.pool.NonBlockingConnectionPool;
import org.davidmoten.rx.jdbc.pool.Pools;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.context.annotation.Profile;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;

@Configuration
@Profile("rxjdbc")
@ConfigurationProperties(prefix = "database")
public class RxJava2Config {
    private String url;
    private String username;
    private String password;

    @Bean
    public Database database() throws SQLException {
        Connection connection = DriverManager.getConnection(url, username, password);
        NonBlockingConnectionPool pool =
                Pools.nonBlocking()
                        .maxPoolSize(Runtime.getRuntime().availableProcessors() * 2)
                        .connectionProvider(ConnectionProvider.from(connection))
                        .build();

        Database db = Database.from(pool);

        return db;
    }
}