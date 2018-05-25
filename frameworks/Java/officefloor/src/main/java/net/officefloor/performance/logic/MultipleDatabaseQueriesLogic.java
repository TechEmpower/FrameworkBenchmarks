package net.officefloor.performance.logic;

import java.io.IOException;
import java.io.Serializable;

import javax.persistence.EntityManager;

import lombok.Data;
import net.officefloor.performance.entities.World;
import net.officefloor.plugin.json.JsonResponseWriter;
import net.officefloor.plugin.web.http.application.HttpParameters;

/**
 * Logic for Multiple Database Queries.
 * 
 * @author Daniel Sagenschneider
 */
public class MultipleDatabaseQueriesLogic {

	@Data
	@HttpParameters
	public static class Parameters implements Serializable {
		private String queries;
	}

	public void service(Parameters parameters, EntityManager entityManager,
			JsonResponseWriter response) throws IOException {

		// Obtain the number of queries
		int queryCount = LogicUtil.getQueryCount(parameters.queries);

		// Create the array of World objects
		World[] list = new World[queryCount];
		for (int i = 0; i < list.length; i++) {
			int identifier = LogicUtil.generateRandomNumber(1, 10000);
			list[i] = entityManager.find(World.class, identifier);
		}

		// Send the response
		response.writeResponse(list);
	}

}