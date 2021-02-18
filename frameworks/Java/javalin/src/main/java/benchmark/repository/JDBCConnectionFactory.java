package benchmark.repository;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;

import java.sql.Connection;
import java.sql.SQLException;


public enum JDBCConnectionFactory {

    INSTANCE;

    private final HikariDataSource ds;

    JDBCConnectionFactory() {
        String propertiesFileName = "/hikari.properties";
        HikariConfig config = new HikariConfig(propertiesFileName);
        int maxPoolSize = DbFactory.INSTANCE.getMaxPoolSize(DbFactory.DbType.POSTGRES);

        ds = new HikariDataSource(config);
        ds.setMaximumPoolSize(maxPoolSize);
    }

    public Connection getConnection() throws SQLException {
        return ds.getConnection();
    }
}
