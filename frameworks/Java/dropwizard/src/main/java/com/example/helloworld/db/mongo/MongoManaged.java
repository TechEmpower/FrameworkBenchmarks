package com.example.helloworld.db.mongo;

import com.mongodb.MongoClient;
import io.dropwizard.lifecycle.Managed;

public class MongoManaged implements Managed {
 
    private MongoClient mongo;
 
    public MongoManaged(MongoClient mongo) {
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