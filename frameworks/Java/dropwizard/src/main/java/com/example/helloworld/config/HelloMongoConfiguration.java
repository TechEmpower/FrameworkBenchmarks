package com.example.helloworld.config;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.dropwizard.Configuration;

import javax.validation.Valid;
import javax.validation.constraints.NotNull;

public class HelloMongoConfiguration extends Configuration {
    @Valid
    @NotNull
    @JsonProperty
    private MongoClientFactory mongo = new MongoClientFactory();

    public MongoClientFactory getMongo() {
        return mongo;
    }
}
