package com.networknt.techempower.db.postgres;

import com.fasterxml.jackson.annotation.JsonIgnore;

/**
 * Created by steve on 10/02/17.
 */
public class PostgresConfig {
    String jdbcUrl;
    String username;
    String password;
    int maximumPoolSize;

    @JsonIgnore
    String description;

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

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }
}
