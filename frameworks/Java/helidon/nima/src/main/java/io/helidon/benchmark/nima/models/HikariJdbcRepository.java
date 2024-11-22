
package io.helidon.benchmark.nima.models;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Logger;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import io.helidon.config.Config;

import static io.helidon.benchmark.nima.models.DbRepository.randomWorldNumber;

public class HikariJdbcRepository implements DbRepository {
    private static final Logger LOGGER = Logger.getLogger(HikariJdbcRepository.class.getName());

    private HikariDataSource ds;
    private final HikariConfig hikariConfig;

    public HikariJdbcRepository(Config config) {
        // hikari connection configuration
        String url = "jdbc:postgresql://" +
                config.get("host").asString().orElse("tfb-database") +
                ":" + config.get("port").asString().orElse("5432") +
                "/" + config.get("db").asString().orElse("hello_world");
        hikariConfig = new HikariConfig();
        hikariConfig.setJdbcUrl(url);
        hikariConfig.setUsername(config.get("username").asString().orElse("benchmarkdbuser"));
        hikariConfig.setPassword(config.get("password").asString().orElse("benchmarkdbpass"));

        // hikari additional configuration
        int poolSize = config.get("sql-pool-size").asInt().orElse(64);
        hikariConfig.setMaximumPoolSize(poolSize);
        LOGGER.info("Hikari pool size is set to " + poolSize);
        ThreadFactory vtThreadFactory = Thread.ofVirtual().factory();
        hikariConfig.setThreadFactory(vtThreadFactory);
        hikariConfig.setScheduledExecutor(Executors.newScheduledThreadPool(poolSize, vtThreadFactory));
        LOGGER.info("Set thread factory to VTs");

        // data source properties
        hikariConfig.addDataSourceProperty("cachePrepStmts","true");
        hikariConfig.addDataSourceProperty("prepStmtCacheSize","250");
        hikariConfig.addDataSourceProperty("prepStmtCacheSqlLimit","2048");
        hikariConfig.addDataSourceProperty("ssl", "false");
        hikariConfig.addDataSourceProperty("tcpKeepAlive", "true");
    }

    private Connection getConnection() throws SQLException {
        if (ds == null) {
            ds = new HikariDataSource(hikariConfig);
        }
        return ds.getConnection();
    }

    @Override
    public World getWorld(int id) {
        try (Connection c = getConnection()) {
            return getWorld(id, c);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> getWorlds(int count) {
        try (Connection c = getConnection()) {
            List<World> result = new ArrayList<>(count);
            for (int i = 0; i < count; i++) {
                result.add(getWorld(randomWorldNumber(), c));
            }
            return result;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<World> updateWorlds(int count) {
        try (Connection c = getConnection()) {
            List<World> result = new ArrayList<>(count);
            for (int i = 0; i < count; i++) {
                World world = getWorld(randomWorldNumber(), c);
                world.randomNumber = randomWorldNumber();
                updateWorld(world, c);
                result.add(world);
            }
            return result;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    public List<Fortune> getFortunes() {
        try (Connection c = getConnection()) {
            List<Fortune> result = new ArrayList<>();
            PreparedStatement ps = c.prepareStatement("SELECT id, message FROM fortune");
            ResultSet rs = ps.executeQuery();
            while (rs.next()) {
                result.add(new Fortune(rs.getInt(1), rs.getString(2)));
            }
            return result;
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private World getWorld(int id, Connection c) throws SQLException {
        PreparedStatement ps = c.prepareStatement("SELECT id, randomnumber FROM world WHERE id = ?");
        ps.setObject(1, id);
        ResultSet rs = ps.executeQuery();
        rs.next();
        World w = new World(rs.getInt(1), rs.getInt(2));
        rs.close();
        return w;
    }

    private World updateWorld(World world, Connection c) throws SQLException {
        PreparedStatement ps = c.prepareStatement("UPDATE world SET randomnumber = ? WHERE id = ?");
        ps.setObject(1, world.randomNumber);
        ps.setObject(2, world.id);
        ps.executeUpdate();
        return world;
    }
}
