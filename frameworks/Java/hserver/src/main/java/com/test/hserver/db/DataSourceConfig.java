package com.test.hserver.db;

import com.zaxxer.hikari.HikariDataSource;
import top.hserver.core.ioc.annotation.Autowired;
import top.hserver.core.ioc.annotation.Bean;
import top.hserver.core.ioc.annotation.Configuration;

import javax.sql.DataSource;

@Configuration
public class DataSourceConfig {

    @Autowired
    private PostgresConfig postgresConfig;

    @Bean
    public DataSource initDataSource() {
        HikariDataSource ds = new HikariDataSource();
        ds.setJdbcUrl(postgresConfig.getJdbcUrl());
        ds.setUsername(postgresConfig.getUsername());
        ds.setPassword(postgresConfig.getPassword());
        ds.setMaximumPoolSize(postgresConfig.getMaximumPoolSize());
        return ds;
    }
}
