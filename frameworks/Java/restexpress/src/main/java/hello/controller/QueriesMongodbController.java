package hello.controller;

import hello.controller.persistence.WorldsMongodbRepository;
import hello.domain.World;

import java.util.concurrent.ThreadLocalRandom;

import org.restexpress.Request;
import org.restexpress.Response;

public class QueriesMongodbController {
	// Database details.
	private static final int DB_ROWS = 10000;

	private WorldsMongodbRepository worldRepo;

	public QueriesMongodbController(WorldsMongodbRepository worldsRepository) {
		super();
		this.worldRepo = worldsRepository;
	}

	public Object read(Request request, Response response) {
		// Get the count of queries to run.
		int count = determineQueryCount(request);
		// Fetch some rows from the database.
		final World[] worlds = new World[count];
		final int random = 1 + ThreadLocalRandom.current().nextInt(DB_ROWS);
		for (int i = 0; i < count; i++) {
			worlds[i] = worldRepo.find(random);
		}

		return worlds;
	}

	private int determineQueryCount(Request request) {
		String value = request.getHeader("queries");

		try {
			int parsedValue = Integer.parseInt(value);
			return Math.min(500, Math.max(1, parsedValue));
		} catch (NumberFormatException e) {
			return 1;
		}
	}
}
