package com.techempower.ee7.tests;

import java.util.List;

import javax.inject.Inject;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import com.techempower.ee7.model.World;
import com.techempower.ee7.util.Helpers;

@Path("/updates")
public class Updates {

    private static final int MIN_QUERIES = 1;
    private static final int MAX_QUERIES = 500;

    @Inject
    private TestActions actions;

    @GET
    @Produces(MediaType.APPLICATION_JSON)
    public List<World> update(@QueryParam("queries") final String queries) {
        final int iterations = Helpers.boundedIntegerFromNullableString(queries, MIN_QUERIES, MAX_QUERIES);
        int attempts = 0;
        while (attempts < 10) {
            try {
                return actions.updateWorlds(iterations);
            }
            catch (Exception e) {
                attempts++;
            }
        }
        throw new RuntimeException(String.format("Unable to update after %s attempts", attempts));
    }
}
