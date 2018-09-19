package hello.helpers;

import com.zaxxer.hikari.HikariConfig;

public final class PostgresDbHelper {
  public static HikariConfig hikariConfig() {
    HikariConfig config = new HikariConfig();
    config.setJdbcUrl("jdbc:postgresql://tfb-database:5432/hello_world");
    config.setUsername("benchmarkdbuser");
    config.setPassword("benchmarkdbpass");
    return config;
  }
}
