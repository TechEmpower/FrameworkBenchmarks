package com.example.helloworld;

import io.dropwizard.Application;
import io.dropwizard.jdbi.DBIFactory;
import io.dropwizard.jdbi.bundles.DBIExceptionsBundle;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import io.dropwizard.views.ViewBundle;

import org.skife.jdbi.v2.DBI;

import com.example.helloworld.config.HelloWorldConfiguration;
import com.example.helloworld.db.jdbi.FortuneJDBIImpl;
import com.example.helloworld.db.jdbi.WorldRepository;
import com.example.helloworld.resources.FortuneResource;
import com.example.helloworld.resources.WorldResource;

public class HelloJDBIService extends Application<HelloWorldConfiguration> {
	public static void main(String[] args) throws Exception {
        new HelloJDBIService().run(args);
    }

    @Override
    public void initialize(Bootstrap<HelloWorldConfiguration> bootstrap) {
        bootstrap.addBundle(new ViewBundle<>());
        bootstrap.addBundle(new DBIExceptionsBundle()); // Provides automatic unwrapping of SQLException and DBIException
    }

    @Override
    public void run(HelloWorldConfiguration config, Environment environment) throws ClassNotFoundException {
    	final DBIFactory factory = new DBIFactory();
        final DBI jdbi = factory.build(environment, config.getDatabaseConfiguration(), "RDBMS");
    	
        // Test type 1: JSON serialization and Test type 6: Plaintext are tested against HelloWorldService class
        environment.jersey().register(new WorldResource(new WorldRepository(jdbi))); // Test types 2, 3 & 5: Single database query, Multiple database queries & Database updates
        environment.jersey().register(new FortuneResource(jdbi.onDemand(FortuneJDBIImpl.class))); // Test type 4: Fortunes 
    }
}
