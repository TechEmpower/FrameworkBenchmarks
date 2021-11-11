package com.example.helloworld;

import com.example.helloworld.db.jdbi.FortuneRepository;
import io.dropwizard.Application;
import io.dropwizard.jdbi3.JdbiFactory;
import io.dropwizard.jdbi3.bundles.JdbiExceptionsBundle;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;
import io.dropwizard.views.ViewBundle;

import org.jdbi.v3.core.Jdbi;

import com.example.helloworld.config.HelloWorldConfiguration;
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
        bootstrap.addBundle(new JdbiExceptionsBundle()); // Provides automatic unwrapping of SQLException and DBIException
    }

    @Override
    public void run(HelloWorldConfiguration config, Environment environment) {
        final JdbiFactory factory = new JdbiFactory();
        final Jdbi jdbi = factory.build(environment, config.getDatabaseConfiguration(), "RDBMS");
    	
        // Test type 1: JSON serialization and Test type 6: Plaintext are tested against HelloWorldService class
        environment.jersey().register(new WorldResource(new WorldRepository(jdbi))); // Test types 2, 3 & 5: Single database query, Multiple database queries & Database updates
        environment.jersey().register(new FortuneResource(new FortuneRepository(jdbi))); // Test type 4: Fortunes
    }
}
