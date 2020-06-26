package hello.controller;

import hello.controller.persistence.WorldsMongodbRepository;

import java.util.concurrent.ThreadLocalRandom;

import org.restexpress.Request;
import org.restexpress.Response;

public class MongodbController {
	// Database details.
	private static final int DB_ROWS = 10000;

	private WorldsMongodbRepository worldRepo;

	public MongodbController(WorldsMongodbRepository worldsRepository) {
		super();
		this.worldRepo = worldsRepository;
	}

	public Object read(Request request, Response response) {
		return worldRepo.find(generateIdentifier());
	}

	private int generateIdentifier() {
		return ThreadLocalRandom.current().nextInt(DB_ROWS) + 1;
	}
}