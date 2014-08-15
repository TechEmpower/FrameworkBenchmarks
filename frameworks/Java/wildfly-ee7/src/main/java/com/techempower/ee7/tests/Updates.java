package com.techempower.ee7.tests;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.persistence.PersistenceException;
import javax.transaction.Transactional;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import org.jboss.logging.Logger;

import com.techempower.ee7.model.World;
import com.techempower.ee7.util.Helpers;

@Path("/updates")
public class Updates {

  private static final int MIN_QUERIES = 1;
  private static final int MAX_QUERIES = 500;

  @Inject
  private EntityManager em;

  @Inject
  private Logger log;

  @Transactional
  @GET
  @Produces(MediaType.APPLICATION_JSON)
  public List<World> update(@QueryParam("queries") final String queries) {
    final int iterations =
        Helpers.boundedIntegerFromNullableString(queries, MIN_QUERIES, MAX_QUERIES);

    List<World> worlds = new ArrayList<>(iterations);

    for (int i = 0; i < iterations; i++) {
      int id = Helpers.randomWorldId();
      worlds.add(em.find(World.class, id));
    }

    for (World w : worlds) {
      w.getRandomNumber(); // Mandatory to read for the test
      w.setRandomNumber(Helpers.randomWorldId());
    }

    try {
      em.joinTransaction();
      em.flush();
    } catch (PersistenceException e) {
      log.info("Failed to flush changes to database.");
      throw e;
    }
    return worlds;
  }
}
