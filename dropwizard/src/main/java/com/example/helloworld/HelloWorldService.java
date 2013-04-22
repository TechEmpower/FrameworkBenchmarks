
package com.example.helloworld;

import com.example.helloworld.core.World;
import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.resources.JsonResource;
import com.example.helloworld.resources.WorldResource;
import com.yammer.dropwizard.Service;
import com.yammer.dropwizard.config.Bootstrap;
import com.yammer.dropwizard.config.Environment;
import com.yammer.dropwizard.db.DatabaseConfiguration;
import com.yammer.dropwizard.hibernate.HibernateBundle;

public class HelloWorldService
    extends Service<HelloWorldConfiguration>
{
  private final HibernateBundle<HelloWorldConfiguration> hibernate = new HibernateBundle<HelloWorldConfiguration>(
                                                                       World.class)
                                                                   {
                                                                     @Override
                                                                     public DatabaseConfiguration getDatabaseConfiguration(
                                                                         HelloWorldConfiguration configuration)
                                                                     {
                                                                       return configuration.getDatabaseConfiguration();
                                                                     }
                                                                   };

  public static void main(String[] args) throws Exception
  {
    new HelloWorldService().run(args);
  }

  @Override
  public void initialize(Bootstrap<HelloWorldConfiguration> bootstrap)
  {
    bootstrap.setName("hello-world");
    bootstrap.addBundle(hibernate);
  }

  @Override
  public void run(HelloWorldConfiguration config, Environment environment)
  {
    final WorldDAO dao = new WorldDAO(hibernate.getSessionFactory());
    environment.addResource(new WorldResource(dao));
    environment.addResource(new JsonResource());
  }

}
