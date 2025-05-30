package benchmark.repository;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import io.avaje.config.Config;
import io.avaje.inject.Bean;
import io.avaje.inject.Factory;
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

    var hikari = new HikariDataSource(new HikariConfig("hikari.properties"));
    hikari.setMaximumPoolSize(maxPoolSize);
    return hikari;
  }
}
