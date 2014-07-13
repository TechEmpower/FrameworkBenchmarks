
package com.example.helloworld;

import java.net.UnknownHostException;

import org.mongojack.JacksonDBCollection;

import com.example.helloworld.core.World;
import com.example.helloworld.db.MongoHealthCheck;
import com.example.helloworld.db.MongoManaged;
import com.example.helloworld.resources.JsonResource;
import com.example.helloworld.resources.WorldResource;
import com.mongodb.DB;
import com.mongodb.Mongo;
import com.yammer.dropwizard.Service;
import com.yammer.dropwizard.config.Bootstrap;
import com.yammer.dropwizard.config.Environment;

public class HelloWorldService
    extends Service<HelloWorldConfiguration>
{

  public static void main(String[] args) throws Exception
  {
    new HelloWorldService().run(args);
  }

  @Override
  public void initialize(Bootstrap<HelloWorldConfiguration> bootstrap)
  {
    bootstrap.setName("hello-world");
  }

  @Override
  public void run(HelloWorldConfiguration config, Environment environment) throws UnknownHostException
  {
    Mongo mongo = new Mongo(config.mongohost, config.mongoport);
    MongoManaged mongoManaged = new MongoManaged(mongo);
    environment.manage(mongoManaged);
    environment.addHealthCheck(new MongoHealthCheck(mongo));
    DB db = mongo.getDB(config.mongodb);
    JacksonDBCollection<World, String> worlds = JacksonDBCollection.wrap(db.getCollection("world"), World.class, String.class);
    environment.addResource(new WorldResource(worlds));
    environment.addResource(new JsonResource());
  }

}
