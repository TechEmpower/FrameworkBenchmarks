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
  public World[] dbTest(@QueryParam("queries") Optional<Integer> queries)
  {
    final int totalQueries = queries.or(1);
    final World[] worlds = new World[queries.or(1)];
    final Random random = new Random(System.currentTimeMillis());

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
