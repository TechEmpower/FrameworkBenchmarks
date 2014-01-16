package com.techempower.ee7.tests;

import javax.inject.Inject;
import javax.persistence.EntityManager;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;

import com.techempower.ee7.model.World;
import com.techempower.ee7.util.Helpers;

@Path("/db")
public class SingleQuery {

  @Inject
  private EntityManager em;

  @GET
  @Produces(MediaType.APPLICATION_JSON)
  public Response get() {
    int id = Helpers.randomWorldId();
    return Helpers.jsonResponse(em.find(World.class, id));
  }
}
