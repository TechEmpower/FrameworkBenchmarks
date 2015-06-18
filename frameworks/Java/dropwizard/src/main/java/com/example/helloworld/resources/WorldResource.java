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
        if (queries.isPresent()) {
            int totalQueries = Helper.getQueries(queries);
            final World[] worlds = new World[totalQueries];
            for (int i = 0; i < totalQueries; i++) {
                worlds[i] = worldDAO.findById(Helper.randomWorld());
            }
            return worlds;
        } else {
            return worldDAO.findById(Helper.randomWorld());
        }
    }

    @GET
    @Path("/update")
    @UnitOfWork(transactional = false)
    public World[] updateTest(@QueryParam("queries") Optional<String> queries) {
        return worldDAO.updatesQueries(Helper.getQueries(queries));
    }
}
