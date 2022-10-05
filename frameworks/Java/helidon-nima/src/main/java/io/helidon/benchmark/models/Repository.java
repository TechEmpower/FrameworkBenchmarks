package io.helidon.benchmark.models;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import io.helidon.config.Config;

import javax.sql.DataSource;
import java.io.PrintWriter;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Properties;


public class Repository {
    private final DataSource ds;

    public Repository(Config config) {
        Properties props = new Properties();
        props.setProperty("dataSourceClassName", "org.postgresql.ds.PGSimpleDataSource");
        props.setProperty("dataSource.serverName", config.get("host").asString().orElse("tfb-database"));
        props.setProperty("dataSource.portNumber", config.get("db.port").asString().orElse("5432"));
        props.setProperty("dataSource.databaseName", config.get("db.name").asString().orElse("hello_world"));
        props.setProperty("dataSource.user", config.get("db.username").asString().orElse("benchmarkdbuser"));
        props.setProperty("dataSource.password", config.get("db.password").asString().orElse("benchmarkdbpass"));

        props.put("dataSource.logWriter", new PrintWriter(System.out));

        var hc = new HikariConfig(props);
        hc.addDataSourceProperty("preparedStatementCacheSizeMiB", "4096");
        hc.addDataSourceProperty("preparedStatementCacheQueries", "2048");
        hc.setMaximumPoolSize(200);
        hc.setMinimumIdle(200);

        ds = new HikariDataSource(hc);
    }

    public World getWorld(int id) {
        try (var conn = ds.getConnection()) {
            try (var stmt = conn.prepareStatement("SELECT id, randomnumber FROM world WHERE id = ?")) {

                stmt.setInt(1, id);
                var rs = stmt.executeQuery();
                if (!rs.next()) throw new RuntimeException("No rows returned in result set.");

                return new World(rs.getInt(1), rs.getInt(2));
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    public void updateWorlds(List<World> worlds) {
        try (var conn = ds.getConnection()) {
            try (var stmt = conn.prepareStatement("UPDATE world SET randomnumber = ? WHERE id = ?")) {

                // Sorting by id to prevent deadlocks.
                worlds.sort(Comparator.comparing(World::id));

                for (var world : worlds) {
                    stmt.setInt(1, world.randomNumber());
                    stmt.setInt(2, world.id());
                    stmt.addBatch();
                }

                stmt.executeBatch();
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }

    public List<Fortune> getFortunes() {
        try (var conn = ds.getConnection()) {
            try (var stmt = conn.prepareStatement("SELECT id, message FROM fortune")) {
                var rs = stmt.executeQuery();

                var fortunes = new ArrayList<Fortune>(100);
                while (rs.next()) {
                    fortunes.add(new Fortune(rs.getInt(1), rs.getString(2)));
                }

                return fortunes;
            }
        } catch (SQLException e) {
            throw new RuntimeException(e);
        }
    }
}