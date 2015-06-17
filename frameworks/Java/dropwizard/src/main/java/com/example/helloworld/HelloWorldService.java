package com.example.helloworld;

import com.example.helloworld.config.HelloWorldConfiguration;
import com.example.helloworld.config.MongoConfiguration;
import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.MongoManaged;
import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.Fortune;
import com.example.helloworld.db.model.World;
import com.example.helloworld.resources.*;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.mongodb.DB;
import com.mongodb.MongoClient;
import io.dropwizard.Application;
import io.dropwizard.db.DataSourceFactory;
import io.dropwizard.hibernate.HibernateBundle;
import io.dropwizard.jackson.Jackson;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import io.dropwizard.views.ViewBundle;
import org.mongojack.JacksonDBCollection;
import org.mongojack.internal.MongoJackModule;

import java.net.UnknownHostException;

public class HelloWorldService extends Application<HelloWorldConfiguration> {

    private final HibernateBundle<HelloWorldConfiguration> hibernate = new HibernateBundle<HelloWorldConfiguration>(World.class, Fortune.class) {
        @Override
        public DataSourceFactory getDataSourceFactory(HelloWorldConfiguration configuration) {
            return configuration.getDatabaseConfiguration();
        }
    };

    public static void main(String[] args) throws Exception {
        new HelloWorldService().run(args);
    }

    @Override
    public void initialize(Bootstrap<HelloWorldConfiguration> bootstrap) {
        bootstrap.addBundle(hibernate);
        bootstrap.addBundle(new ViewBundle());
    }

    @Override
    public void run(HelloWorldConfiguration config, Environment environment) throws UnknownHostException {
        environment.jersey().register(new JsonResource()); // Test type 1: JSON serialization
        environment.jersey().register(new WorldResource(new WorldDAO(hibernate.getSessionFactory()))); // Test types 2, 3 & 5: Single database query, Multiple database queries & Database updates
        environment.jersey().register(new FortuneResource(new FortuneDAO(hibernate.getSessionFactory()))); // Test type 4: Fortunes
        environment.jersey().register(new TextResource()); // Test type 6: Plaintext

        setUpMongo(config.getMongo(), environment);
    }

    private void setUpMongo(MongoConfiguration config, Environment environment) throws UnknownHostException {
        final MongoClient mongoClient = new MongoClient(config.getHost(), config.getPort());
        environment.lifecycle().manage(new MongoManaged(mongoClient));

        final DB db = mongoClient.getDB(config.getDb());
        final ObjectMapper mongoJackMapper = MongoJackModule.configure(Jackson.newObjectMapper());
        final JacksonDBCollection<World, Long> worlds = JacksonDBCollection.wrap(
                db.getCollection("World"),
                World.class,
                Long.class,
                mongoJackMapper);

        final JacksonDBCollection<Fortune, Long> fortunes = JacksonDBCollection.wrap(
                db.getCollection("Fortune"),
                Fortune.class,
                Long.class,
                mongoJackMapper);

        environment.jersey().register(new MongoWorldResource(worlds));
        environment.jersey().register(new MongoFortuneResource(fortunes));
    }
}
