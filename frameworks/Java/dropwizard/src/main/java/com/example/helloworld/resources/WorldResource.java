package com.example.helloworld.resources;

import io.dropwizard.hibernate.UnitOfWork;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;
import com.google.common.base.Optional;

@Path("/db")
@Produces(MediaType.APPLICATION_JSON)
public class WorldResource {
    private final WorldDAO worldDAO;

    public WorldResource(WorldDAO worldDAO) {
        this.worldDAO = worldDAO;
    }

    @GET
    @UnitOfWork
    public Object dbTest(@QueryParam("queries") Optional<String> queries) {
    	int totalQueries = Helper.getQueries(queries);
        final World[] worlds = new World[totalQueries];

        // TODO: Is parallelising this cheating?
        for (int i = 0; i < totalQueries; i++) {
            final long worldId = Helper.randomWorld();
            worlds[i] = worldDAO.findById(worldId).orNull();
        }
        if (!queries.isPresent()) {
        	return worlds[0];
        } else {
        	return worlds;
        }
    }

    @GET
    @Path("/update")
    @UnitOfWork
    public World[] updateTest(@QueryParam("queries") Optional<String> queries) {
        int totalQueries = Helper.getQueries(queries);
        final World[] worlds = new World[totalQueries];

        // TODO: Is parallelising this cheating?
        for (int i = 0; i < totalQueries; i++) {
            final long worldId = Helper.randomWorld();

            final World world = worldDAO.findById(worldId).orNull();
            world.setRandomNumber(Helper.randomWorld());
            worlds[i] = worldDAO.update(world);
        }

        return worlds;
    }
}
