package com.example.helloworld.resources;

import java.util.Random;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import com.example.helloworld.core.World;
import com.example.helloworld.db.WorldDAO;
import com.google.common.base.Optional;
import com.yammer.dropwizard.hibernate.UnitOfWork;

@Path("/db")
@Produces(MediaType.APPLICATION_JSON)
public class WorldResource
{
  private WorldDAO worldDAO = null;

  public WorldResource(WorldDAO worldDAO)
  {
    this.worldDAO = worldDAO;
  }

  @GET
  @UnitOfWork
  public World[] dbTest(@QueryParam("queries") Optional<Integer> queries)
  {
    final int totalQueries = queries.or(1);
    final World[] worlds = new World[queries.or(1)];
    final Random random = new Random(System.currentTimeMillis());

    for (int i = 0; i < totalQueries; i++)
    {
      worlds[i] = this.worldDAO.findById((long)(random.nextInt(10000) + 1)).orNull();
    }
    return worlds;
  }
}
