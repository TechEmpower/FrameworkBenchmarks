
package com.example.helloworld.config;

import com.fasterxml.jackson.annotation.JsonProperty;
import io.dropwizard.Configuration;

public class HelloWorldConfiguration extends Configuration {
    @JsonProperty
    public MongoConfiguration mongo;


}
