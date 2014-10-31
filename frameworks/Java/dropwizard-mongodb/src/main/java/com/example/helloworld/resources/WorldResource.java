package com.example.helloworld.resources;

import java.util.Random;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.mongojack.DBCursor;
import org.mongojack.JacksonDBCollection;

import com.example.helloworld.core.World;
import com.google.common.base.Optional;
import com.mongodb.BasicDBObject;
import com.mongodb.DBObject;

@Path("/db")
@Produces(MediaType.APPLICATION_JSON)
public class WorldResource
{

  private JacksonDBCollection<World, String> collection;
 
  public WorldResource(JacksonDBCollection<World, String> collection)
  {
    this.collection = collection;
  }

  @GET
  public Object dbTest(@QueryParam("queries") Optional<String> queries)
  {
    final Random random = new Random(System.currentTimeMillis());
    if (!queries.isPresent()) 
    {
      DBObject query = new BasicDBObject();
      query.put("_id", (random.nextInt(10000) + 1));
      DBCursor<World> dbCursor = collection.find(query);
      return (dbCursor.hasNext()) ? dbCursor.next() : null;
    }
    Integer totalQueries = Ints.tryParse(queries.orNull());
    if (totalQueries != null) 
    {
      if (totalQueries > 500) 
      {
        totalQueries = 500;
      }
      else if (totalQueries < 1) 
      {
        totalQueries = 1;
      }
    } 
    else 
    {
      totalQueries = 1;
    }
    final World[] worlds = new World[totalQueries];
    for (int i = 0; i < totalQueries; i++)
    {
    	DBObject query = new BasicDBObject();
    	query.put("_id", (random.nextInt(10000) + 1));
    	DBCursor<World> dbCursor = collection.find(query);
      worlds[i] = (dbCursor.hasNext()) ? dbCursor.next() : null;
    }
    return worlds;
  }
}
