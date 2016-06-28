package com.example.helloworld;

import com.example.helloworld.config.HelloMongoConfiguration;
import com.example.helloworld.db.model.Fortune;
import com.example.helloworld.db.model.World;
import com.example.helloworld.db.mongo.FortuneMongoImpl;
import com.example.helloworld.db.mongo.MongoManaged;
import com.example.helloworld.db.mongo.WorldMongoImpl;
import com.example.helloworld.resources.FortuneResource;
import com.example.helloworld.resources.WorldResource;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mongodb.DB;
import com.mongodb.MongoClient;
import io.dropwizard.Application;
import io.dropwizard.jackson.Jackson;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import io.dropwizard.views.ViewBundle;
import org.mongojack.JacksonDBCollection;
import org.mongojack.internal.MongoJackModule;

import java.net.UnknownHostException;

public class HelloMongoService extends Application<HelloMongoConfiguration> {

    public static void main(String[] args) throws Exception {
        new HelloMongoService().run(args);
    }

    @Override
    public void initialize(Bootstrap<HelloMongoConfiguration> bootstrap) {
        bootstrap.addBundle(new ViewBundle());
    }

    @Override
    public void run(HelloMongoConfiguration config, Environment environment) throws UnknownHostException {
        final MongoClient mongoClient = config.getMongo().build();
        environment.lifecycle().manage(new MongoManaged(mongoClient));

        final DB db = mongoClient.getDB(config.getMongo().getDb());
        final ObjectMapper mongoJackMapper = MongoJackModule.configure(Jackson.newObjectMapper());
        final JacksonDBCollection<World, Integer> worlds =
                JacksonDBCollection.wrap(db.getCollection("World"), World.class, Integer.class, mongoJackMapper);

        final JacksonDBCollection<Fortune, Integer> fortunes =
                JacksonDBCollection.wrap(db.getCollection("Fortune"), Fortune.class, Integer.class, mongoJackMapper);

        environment.jersey().register(new WorldResource(new WorldMongoImpl(worlds))); // Test types 2, 3 & 5: Single database query, Multiple database queries & Database updates
        environment.jersey().register(new FortuneResource(new FortuneMongoImpl(fortunes))); // Test type 4: Fortunes
    }

}
