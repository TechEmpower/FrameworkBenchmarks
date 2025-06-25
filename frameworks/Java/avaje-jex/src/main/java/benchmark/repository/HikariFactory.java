package benchmark.repository;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import io.avaje.config.Config;
import io.avaje.inject.Bean;
import io.avaje.inject.Factory;
import java.util.concurrent.Executors;
import javax.sql.DataSource;

@Factory
public class HikariFactory {

  @Bean
  DataSource dataSource() {
    int maxPoolSize;
    var env = System.getenv("BENCHMARK_ENV");
    if (Config.get("physicalTag").equals(env)) {
      maxPoolSize = Config.getInt("postgresPhysicalPoolSize");
    } else if (Config.get("cloudTag").equals(env)) {
      maxPoolSize = Config.getInt("postgresCloudPoolSize");
    } else {
      maxPoolSize = Config.getInt("postgresDefaultPoolSize");
    }

    maxPoolSize = Math.max(maxPoolSize, Runtime.getRuntime().availableProcessors() * 2);
    HikariConfig hikariConfig = new HikariConfig("hikari.properties");

    var vtThreadFactory = Thread.ofVirtual().factory();
    hikariConfig.setThreadFactory(vtThreadFactory);
    hikariConfig.setScheduledExecutor(
        Executors.newScheduledThreadPool(maxPoolSize, vtThreadFactory));

    // data source properties
    hikariConfig.addDataSourceProperty("cachePrepStmts", "true");
    hikariConfig.addDataSourceProperty("prepStmtCacheSize", "250");
    hikariConfig.addDataSourceProperty("prepStmtCacheSqlLimit", "2048");
    hikariConfig.addDataSourceProperty("ssl", "false");
    hikariConfig.addDataSourceProperty("tcpKeepAlive", "true");
    hikariConfig.setMaximumPoolSize(maxPoolSize);
    return new HikariDataSource(hikariConfig);
  }
}
