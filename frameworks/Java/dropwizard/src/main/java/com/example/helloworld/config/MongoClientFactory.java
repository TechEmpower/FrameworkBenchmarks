package com.example.helloworld.config;

import javax.validation.constraints.Max;
import javax.validation.constraints.Min;

import org.hibernate.validator.constraints.NotEmpty;

import com.fasterxml.jackson.annotation.JsonProperty;
import com.mongodb.MongoClient;
import com.mongodb.MongoClientOptions;
import com.mongodb.ServerAddress;

public class MongoClientFactory {
	private static final int MAX_DB_REQUEST_CONCURRENCY = 512;
	
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
    
    @JsonProperty
    @Min(1)
    @Max(4096)
    private int connections;

    public String getHost() {
        return host;
    }

    public int getPort() {
        return port;
    }

    public String getDb() {
        return db;
    }

    public int getConnections() {
    	return connections;
    }
    
    public MongoClient build() {
    	MongoClientOptions.Builder options = MongoClientOptions.builder();
        options.connectionsPerHost(connections);
        options.threadsAllowedToBlockForConnectionMultiplier(
            (int) Math.ceil((double) MAX_DB_REQUEST_CONCURRENCY / connections));
        return new MongoClient(new ServerAddress(host, port), options.build());
    }
}
