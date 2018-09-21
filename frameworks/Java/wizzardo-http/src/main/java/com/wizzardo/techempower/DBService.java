package com.wizzardo.techempower;

import com.wizzardo.http.framework.Configuration;
import com.wizzardo.http.framework.di.Service;
import io.reactiverse.pgclient.PgPool;
import io.reactiverse.pgclient.impl.WizzardoPgPool;
import io.reactiverse.pgclient.impl.WizzardoPgPoolOptions;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;

public class DBService implements Service {

    protected DBConfig config;
    protected DataSource dataSource;

    protected ThreadLocal<PgPool> clientThreadLocal = ThreadLocal.withInitial(this::initPgPool);

    protected PgPool initPgPool() {
        WizzardoPgPoolOptions options = new WizzardoPgPoolOptions();
        options.setDatabase(config.dbname);
        options.setHost(config.host);
        options.setPort(config.port);
        options.setUser(config.username);
        options.setPassword(config.password);
        options.setCachePreparedStatements(true);
        options.setMaxSize(1);
        return new WizzardoPgPool(options);
    }

    public Connection getConnection() throws SQLException {
        return dataSource.getConnection();
    }

    public PgPool getClient() {
        return clientThreadLocal.get();
    }

    public static class DBConfig implements Configuration {
        public final String dbname;
        public final String host;
        public final int port;
        public final String username;
        public final String password;
        public final int maximumPoolSize;
        public final int minimumIdle;

        public DBConfig() {
            dbname = null;
            username = null;
            password = null;
            maximumPoolSize = 0;
            minimumIdle = 0;
            host = null;
            port = 0;
        }


        @Override
        public String prefix() {
            return "db";
        }

        @Override
        public String toString() {
            return "DBConfig{" +
                    "dbname='" + dbname + '\'' +
                    ", host='" + host + '\'' +
                    ", port=" + port +
                    ", username='" + username + '\'' +
                    ", password='" + password + '\'' +
                    ", maximumPoolSize=" + maximumPoolSize +
                    ", minimumIdle=" + minimumIdle +
                    '}';
        }
    }
}
