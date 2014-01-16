package com.techempower.ee7.tests;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.techempower.ee7.model.World;
import com.techempower.ee7.util.Helpers;

@Path("/queries")
public class MultipleQueries {

  private static final int MIN_QUERIES = 1;
  private static final int MAX_QUERIES = 500;

  @Inject
  private EntityManager em;

  @GET
  @Produces(MediaType.APPLICATION_JSON)
  public Response get(@QueryParam("queries") final String queries) {
    final int iterations =
        Helpers.boundedIntegerFromNullableString(queries, MIN_QUERIES, MAX_QUERIES);

    List<World> result = new ArrayList<>(iterations);

    for (int i = 0; i < iterations; i++) {
      int id = Helpers.randomWorldId();
      result.add(em.find(World.class, id));
    }

    return Helpers.jsonResponse(result);
  }
}
