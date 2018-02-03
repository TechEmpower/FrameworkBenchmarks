package com.example.helloworld.resources;

import java.util.Optional;

import io.dropwizard.hibernate.UnitOfWork;

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.QueryParam;
import javax.ws.rs.core.MediaType;

import com.example.helloworld.db.WorldDAO;
import com.example.helloworld.db.model.World;

@Path("/db")
@Produces(MediaType.APPLICATION_JSON)
public class WorldResource {
	private final WorldDAO worldDAO;

	public WorldResource(WorldDAO worldDAO) {
		this.worldDAO = worldDAO;
	}
	
	@GET
	@UnitOfWork(transactional = false, readOnly = true) // Needed only for Hibernate - not for Mongo or JDBI
	public Object db() {
		return worldDAO.findById(Helper.randomWorld());
	}
	
	@GET
	@Path("/query")
	@UnitOfWork(transactional = false,  readOnly = true) // Needed only for Hibernate - not for Mongo or JDBI
	public Object dbTest(@QueryParam("queries") String queries) {
		int totalQueries = Helper.getQueries(queries); // Optional check is done inside
		final World[] worlds = new World[totalQueries];
		for (int i = 0; i < totalQueries; i++) {
			worlds[i] = worldDAO.findById(Helper.randomWorld());
		}
		return worlds;
	}

	@GET
	@Path("/update")
	@UnitOfWork(transactional = false) // Needed only for Hibernate - not for Mongo or JDBI
	public World[] updateTest(@QueryParam("queries") Optional<String> queries) {
		return worldDAO.updatesQueries(Helper.getQueries(queries));
	}
}