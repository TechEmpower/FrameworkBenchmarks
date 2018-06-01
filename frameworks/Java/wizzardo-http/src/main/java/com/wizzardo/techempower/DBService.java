package com.wizzardo.techempower;

import com.wizzardo.http.framework.Configuration;
import com.wizzardo.http.framework.di.PostConstruct;
import com.wizzardo.http.framework.di.Service;

import com.zaxxer.hikari.HikariConfig;
import com.zaxxer.hikari.HikariDataSource;
import org.postgresql.ds.PGSimpleDataSource;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.SQLException;

public class DBService implements Service, PostConstruct {

    protected DBConfig config;
    protected DataSource dataSource;

    @Override
    public void init() {
        PGSimpleDataSource source = new PGSimpleDataSource();
        source.setUrl("jdbc:postgresql://" + config.host + ":" + config.port + "/" + config.dbname);
        source.setUser(config.username);
        source.setPassword(config.password);

        HikariConfig hikariConfig = new HikariConfig();
        hikariConfig.setMaximumPoolSize(config.maximumPoolSize);
        hikariConfig.setMinimumIdle(config.minimumIdle);
        hikariConfig.setDataSource(source);

        dataSource = new HikariDataSource(hikariConfig);
    }

    public Connection getConnection() throws SQLException {
        return dataSource.getConnection();
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
