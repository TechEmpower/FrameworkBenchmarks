package io.quarkus.benchmark.resource;

import javax.inject.Inject;
import javax.inject.Singleton;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import io.quarkus.benchmark.model.World;
import io.quarkus.benchmark.repository.WorldRepository;
import io.quarkus.benchmark.utils.Randomizer;

@Singleton
@Path("/")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class DbResource {

    @Inject
    WorldRepository worldRepository;

    @GET
    @Path("/db")
    public World db() {
        return worldRepository.loadSingleWorldById(Randomizer.current().getNextRandom());
    }

    @GET
    @Path("/queries")
    public World[] queries(@QueryParam("queries") String queries) {
        final int count = parseQueryCount(queries);
        return worldRepository.loadNWorlds(count);
    }

    @GET
    @Path("/updates")
    //Rules: https://github.com/TechEmpower/FrameworkBenchmarks/wiki/Project-Information-Framework-Tests-Overview#database-updates
    //N.B. the benchmark seems to be designed to get in deadlocks when using a "safe pattern" of updating
    // the entity within the same transaction as the one which read it.
    // We therefore need to do a "read then write" while relinquishing the transaction between the two operations, as
    // all other tested frameworks seem to do.
    public World[] updates(@QueryParam("queries") String queries) {
        final int count = parseQueryCount(queries);
        return worldRepository.updateNWorlds(count);
    }

    @GET
    @Path( "/createdata" )
    public String createData() {
        worldRepository.createData();
        return "OK";
    }

    private int parseQueryCount(final String textValue) {
        if (textValue == null) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}
