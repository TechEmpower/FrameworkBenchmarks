package com.example.helloworld.config;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.hibernate.validator.constraints.NotEmpty;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

public class MongoConfiguration {
    @JsonProperty
    @NotEmpty
    private String host;

    @JsonProperty
    @Min(1)
    @Max(65535)
    private int port;

    @JsonProperty
    @NotEmpty
    private String db;

    public String getHost() {
        return host;
    }

    public int getPort() {
        return port;
    }

    public String getDb() {
        return db;
    }
}
