package com.test.hserver.db;


import cn.hserver.core.ioc.annotation.ConfigurationProperties;

@ConfigurationProperties
public class PostgresConfig {
    private String jdbcUrl;
    private String username;
    private String password;
    private int maximumPoolSize;

    public PostgresConfig() {
    }

    public String getJdbcUrl() {
        return jdbcUrl;
    }

    public void setJdbcUrl(String jdbcUrl) {
        this.jdbcUrl = jdbcUrl;
    }

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }

    public int getMaximumPoolSize() {
        return maximumPoolSize;
    }

    public void setMaximumPoolSize(int maximumPoolSize) {
        this.maximumPoolSize = maximumPoolSize;
    }

    @Override
    public String toString() {
        return "PostgresConfig{" +
                "jdbcUrl='" + jdbcUrl + '\'' +
                ", username='" + username + '\'' +
                ", password='" + password + '\'' +
                ", maximumPoolSize=" + maximumPoolSize +
                '}';
    }
}