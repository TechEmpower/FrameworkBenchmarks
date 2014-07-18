package com.example.helloworld.resources;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.google.common.base.Optional;
import io.dropwizard.hibernate.UnitOfWork;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;
import java.util.Random;

@Path("/db")
@Produces(MediaType.APPLICATION_JSON)
public class WorldResource {

    private static final Random RANDOM = new Random();

    private final WorldDAO worldDAO;

    public WorldResource(WorldDAO worldDAO) {
        this.worldDAO = worldDAO;
    }

    @GET
    @UnitOfWork
    public World[] dbTest(@QueryParam("queries") Optional<Integer> queries) {
        final int totalQueries = queries.or(1); // TODO: Should be bound [1,500]
        final World[] worlds = new World[totalQueries];

        // TODO: Is parallelising this cheating?
        for (int i = 0; i < totalQueries; i++) {
            final long worldId = RANDOM.nextInt(10_000) + 1;
            worlds[i] = worldDAO.findById(worldId).orNull();
        }

        return worlds;
    }

    @GET
    @Path("/update")
    @UnitOfWork
    public World[] updateTest(@QueryParam("queries") Optional<Integer> queries) {
        final int totalQueries = queries.or(1); // TODO: Should be bound [1,500]
        final World[] worlds = new World[totalQueries];

        // TODO: Is parallelising this cheating?
        for (int i = 0; i < totalQueries; i++) {
            final long worldId = RANDOM.nextInt(10_000) + 1;

            final World world = worldDAO.findById(worldId).orNull();
            world.setRandomNumber(RANDOM.nextInt(10_000) + 1);
            worlds[i] = worldDAO.update(world);
        }

        return worlds;
    }
}
