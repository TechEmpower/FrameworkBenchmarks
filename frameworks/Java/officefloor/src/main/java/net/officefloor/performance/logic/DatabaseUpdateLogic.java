package net.officefloor.performance.logic;

import java.io.IOException;
import java.io.Serializable;
import java.util.Arrays;

import javax.persistence.EntityManager;

import lombok.Data;
import net.officefloor.performance.entities.World;
import net.officefloor.plugin.json.JsonResponseWriter;
import net.officefloor.plugin.web.http.application.HttpParameters;

/**
 * Logic for Database Update query.
 * 
 * @author Daniel Sagenschneider
 */
public class DatabaseUpdateLogic {

	@Data
	@HttpParameters
	public static class Parameters implements Serializable {
		private String queries;
	}

	public void service(Parameters parameters, EntityManager entityManager,
			JsonResponseWriter response) throws IOException {

		// Obtain the number of queries
		int queryCount = LogicUtil.getQueryCount(parameters.queries);

		// Create the listing of random identifiers
		int[] identifiers = new int[queryCount];
		for (int i = 0; i < identifiers.length; i++) {
			identifiers[i] = LogicUtil.generateRandomNumber(1, 10000);
		}

		// Sort identifiers to avoid dead locks
		Arrays.sort(identifiers);

		// Obtain the world objects (changing their random values)
		World[] list = new World[queryCount];
		for (int i = 0; i < list.length; i++) {

			// Obtain the object
			int identifier = identifiers[i];
			World world = entityManager.find(World.class, identifier);
			list[i] = world;

			// Change the random value
			world.setRandomNumber(LogicUtil.generateRandomNumber(1, 10000));
		}

		// Send the response
		response.writeResponse(list);
	}

}