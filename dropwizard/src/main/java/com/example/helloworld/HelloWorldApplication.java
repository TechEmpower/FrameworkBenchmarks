
package com.example.helloworld;

import com.example.helloworld.core.World;
import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.resources.JsonResource;
import com.example.helloworld.resources.WorldResource;
import io.dropwizard.Application;
import io.dropwizard.db.DataSourceFactory;
import io.dropwizard.hibernate.HibernateBundle;
import io.dropwizard.setup.Bootstrap;
import io.dropwizard.setup.Environment;

public class HelloWorldApplication
    extends Application<HelloWorldConfiguration>
{

  private final HibernateBundle<HelloWorldConfiguration> hibernate =
          new HibernateBundle<HelloWorldConfiguration>(World.class) {
      @Override
      public DataSourceFactory getDataSourceFactory(HelloWorldConfiguration configuration) {
          return configuration.getDataSourceFactory();
      }
  };

  public static void main(String[] args) throws Exception
  {
    new HelloWorldApplication().run(args);
  }

    @Override
    public String getName() {
        return "hello-world";
    }

    @Override
  public void initialize(Bootstrap<HelloWorldConfiguration> bootstrap)
  {
    bootstrap.addBundle(hibernate);
  }

  @Override
  public void run(HelloWorldConfiguration config, Environment environment)
  {
    final WorldDAO dao = new WorldDAO(hibernate.getSessionFactory());
    environment.jersey().register(new WorldResource(dao));

    environment.jersey().register(new JsonResource());
  }

}
