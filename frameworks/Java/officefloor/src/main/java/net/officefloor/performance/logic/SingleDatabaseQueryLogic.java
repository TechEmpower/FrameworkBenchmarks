package net.officefloor.performance.logic;

import java.io.IOException;

import javax.persistence.EntityManager;

import net.officefloor.performance.entities.World;
import net.officefloor.plugin.json.JsonResponseWriter;

/**
 * Logic for Single Database Query.
 * 
 * @author Daniel Sagenschneider
 */
public class SingleDatabaseQueryLogic {

	public void service(EntityManager entityManager, JsonResponseWriter response)
			throws IOException {

		// Obtain the identifier
		int identifier = LogicUtil.generateRandomNumber(1, 10000);

		// Retrieve the row
		World world = entityManager.find(World.class, identifier);

		// Write the response
		response.writeResponse(world);
	}

}