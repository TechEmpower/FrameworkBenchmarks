package com.techempower.ee7.tests;

import java.util.ArrayList;
import java.util.List;

import jakarta.inject.Inject;
import jakarta.persistence.EntityManager;
import jakarta.ws.rs.GET;
import jakarta.ws.rs.Path;
import jakarta.ws.rs.Produces;
import jakarta.ws.rs.QueryParam;
import jakarta.ws.rs.core.MediaType;

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
    public List<World> get(@QueryParam("queries") final String queries) {
        final int iterations = Helpers.boundedIntegerFromNullableString(queries, MIN_QUERIES, MAX_QUERIES);

        List<World> result = new ArrayList<>(iterations);

        for (int i = 0; i < iterations; i++) {
            int id = Helpers.randomWorldId();
            result.add(em.find(World.class, id));
        }

        return result;
    }
}
