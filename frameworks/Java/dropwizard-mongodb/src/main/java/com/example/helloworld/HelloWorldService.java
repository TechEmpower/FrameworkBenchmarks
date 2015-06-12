
package com.example.helloworld;

import com.example.helloworld.config.HelloWorldConfiguration;
import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.MongoHealthCheck;
import com.example.helloworld.db.MongoManaged;
import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.resources.FortuneResource;
import com.example.helloworld.resources.JsonResource;
import com.example.helloworld.resources.TextResource;
import com.example.helloworld.resources.WorldResource;
import com.mongodb.DB;
import com.mongodb.MongoClient;
import io.dropwizard.Application;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

import java.net.UnknownHostException;

public class HelloWorldService
        extends Application<HelloWorldConfiguration> {

    public static void main(String[] args) throws Exception {
        new HelloWorldService().run(args);
    }

    @Override
    public void run(HelloWorldConfiguration config, Environment environment) throws UnknownHostException {
        MongoClient mongoClient = new MongoClient( "localhost" , 27017 );
        environment.lifecycle().manage(new MongoManaged(mongoClient));
        environment.healthChecks().register("mongo", new MongoHealthCheck(mongoClient));

        DB db = mongoClient.getDB(config.mongo.db);
        WorldDAO worldDAO = new WorldDAO(db);
        FortuneDAO fortuneDAO = new FortuneDAO(db);

        environment.jersey().register(new JsonResource());
        environment.jersey().register(new WorldResource(worldDAO));
        environment.jersey().register(new FortuneResource(fortuneDAO));
        environment.jersey().register(new TextResource());
    }

    @Override
    public void initialize(Bootstrap<HelloWorldConfiguration> bootstrap) {

    }
}
