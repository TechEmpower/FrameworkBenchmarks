package db;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.Duration;
import java.util.Properties;

public class DbConnection {

    private static final Properties PROPERTIES = getProperties();
    private static final String URL = "jdbc:mysql://tfb-database:3306/hello_world";
    private static final String SELECT = "select id, randomNumber from world where id = ?";

    private static Properties getProperties() {
        Properties properties = new Properties();
        properties.put("user", "benchmarkdbuser");
        properties.put("password", "benchmarkdbpass");
        properties.put("useSSL", "false");
        return properties;
    }

    private final Connection connection;
    private final PreparedStatement statement;

    private long lastUse;

    public DbConnection() throws SQLException {
        this.connection = DriverManager.getConnection(URL, PROPERTIES);
        this.statement = connection.prepareStatement(SELECT);
        this.lastUse = System.nanoTime();
    }

    public WorldRow executeQuery(int id) throws SQLException {
        lastUse = System.nanoTime();
        statement.setInt(1, id);
        try (ResultSet rs = statement.executeQuery()) {
            rs.next();
            return new WorldRow(rs.getInt(1), rs.getInt(2));
        }
    }

    public boolean isIdle(Duration maxIdle) {
        return System.nanoTime() - lastUse > maxIdle.toNanos();
    }

    public void close() {
        try {
            statement.close();
        } catch (SQLException ignore) {}
        try {
            connection.close();
        } catch (SQLException ignore) {}
    }

}
