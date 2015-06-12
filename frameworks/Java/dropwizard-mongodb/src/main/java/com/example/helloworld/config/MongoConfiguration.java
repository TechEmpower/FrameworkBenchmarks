package com.example.helloworld.config;

import com.fasterxml.jackson.annotation.JsonProperty;
import org.hibernate.validator.constraints.NotEmpty;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

public class MongoConfiguration {
    @JsonProperty
    @NotEmpty
    public String host;

    @JsonProperty
    @Min(1)
    @Max(65535)
    public int port;

    @JsonProperty
    @NotEmpty
    public String db;
}
