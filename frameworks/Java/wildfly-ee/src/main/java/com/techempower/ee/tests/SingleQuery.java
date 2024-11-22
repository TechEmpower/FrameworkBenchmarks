package com.techempower.ee7.tests;

import jakarta.inject.Inject;
import jakarta.persistence.EntityManager;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.core.MediaType;

import com.techempower.ee7.model.World;
import com.techempower.ee7.util.Helpers;

@Path("/db")
public class SingleQuery {

    @Inject
    private EntityManager em;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public World get() {
        return em.find(World.class, Helpers.randomWorldId());
    }
}
