package io.quarkus.benchmark.resource;

import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

import javax.enterprise.context.ApplicationScoped;
import javax.inject.Inject;
import javax.ws.rs.Consumes;
import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import io.quarkus.benchmark.model.World;
import io.quarkus.benchmark.repository.WorldRepository;

@ApplicationScoped
@Path("/")
@Produces(MediaType.APPLICATION_JSON)
@Consumes(MediaType.APPLICATION_JSON)
public class DbResource {

    @Inject
    WorldRepository worldRepository;

    @GET
    @Path("/db")
    public World db() {
        return randomWorld();
    }

    @GET
    @Path("/queries")
    public World[] queries(@QueryParam("queries") String queries) {
        var worlds = new World[parseQueryCount(queries)];
        Arrays.setAll(worlds, i -> randomWorld());
        return worlds;
    }

    @GET
    @Path("/updates")
    public World[] updates(@QueryParam("queries") String queries) {
        var worlds = new World[parseQueryCount(queries)];
        Arrays.setAll(worlds, i -> {
            World world = randomWorld();
            world.setRandomNumber(randomWorldNumber());
            return world;
        });

        worldRepository.update(worlds);

        return worlds;
    }

    private World randomWorld() {
        return worldRepository.find(randomWorldNumber());
    }

    private int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private int parseQueryCount(String textValue) {
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
