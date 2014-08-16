package com.example.helloworld.db;

import com.mongodb.Mongo;
import com.yammer.dropwizard.lifecycle.Managed;
 
public class MongoManaged implements Managed {
 
    private Mongo mongo;
 
    public MongoManaged(Mongo mongo) {
        this.mongo = mongo;
    }
 
    @Override
    public void start() throws Exception {
    }
 
    @Override
    public void stop() throws Exception {
        mongo.close();
    }
 
}