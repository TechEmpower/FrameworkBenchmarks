package com.example.helloworld;

import com.example.helloworld.config.HelloWorldConfiguration;
import com.example.helloworld.db.FortuneDAO;
import com.example.helloworld.db.model.Fortune;
import com.example.helloworld.db.model.World;
import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.resources.FortuneResource;
import com.example.helloworld.resources.JsonResource;
import com.example.helloworld.resources.TextResource;
import com.example.helloworld.resources.WorldResource;
import io.dropwizard.Application;
import io.dropwizard.db.DataSourceFactory;
import io.dropwizard.hibernate.HibernateBundle;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import io.dropwizard.views.ViewBundle;

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
    public void run(HelloWorldConfiguration config, Environment environment) {
        environment.jersey().register(new JsonResource()); // Test type 1: JSON serialization
        environment.jersey().register(new WorldResource(new WorldDAO(hibernate.getSessionFactory()))); // Test types 2, 3 & 5: Single database query, Multiple database queries & Database updates
        environment.jersey().register(new FortuneResource(new FortuneDAO(hibernate.getSessionFactory()))); // Test type 4: Fortunes
        environment.jersey().register(new TextResource()); // Test type 6: Plaintext
    }
}
