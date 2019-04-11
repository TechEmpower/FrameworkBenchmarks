package hello.controller;

import hello.controller.persistence.WorldsMongodbRepository;

import java.util.concurrent.ThreadLocalRandom;

import com.strategicgains.restexpress.Request;
import com.strategicgains.restexpress.Response;

public class DbMongodbController {
	// Database details.
	private static final int DB_ROWS = 10000;

	private WorldsMongodbRepository worldRepo;

	public DbMongodbController(WorldsMongodbRepository worldsRepository) {
		super();
		this.worldRepo = worldsRepository;
	}

	public Object read(Request request, Response response) {
		// Fetch some rows from the database.
		final int random = 1 + ThreadLocalRandom.current().nextInt(DB_ROWS);

		return worldRepo.find(random);
	}
}
