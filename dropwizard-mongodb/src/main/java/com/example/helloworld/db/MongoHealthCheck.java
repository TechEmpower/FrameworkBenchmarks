package com.example.helloworld.db;

import com.mongodb.Mongo;
import com.yammer.metrics.core.HealthCheck;
 
public class MongoHealthCheck extends HealthCheck {
 
    private Mongo mongo;
 
    public MongoHealthCheck(Mongo mongo) {
        super("MongoDBHealthCheck");
        this.mongo = mongo;
    }
 
    @Override
    protected Result check() throws Exception {
        mongo.getDatabaseNames();
        return Result.healthy();
    }
 
}
