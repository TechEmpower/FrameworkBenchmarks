package hello.controller;

import hello.controller.persistence.WorldsMongodbRepository;
import hello.domain.World;

import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import com.strategicgains.restexpress.Request;
import com.strategicgains.restexpress.Response;

public class MongodbController
{
	// Database details.
	private static final int DB_ROWS = 10000;

	private WorldsMongodbRepository worldRepo;

	public MongodbController(WorldsMongodbRepository worldsRepository)
	{
		super();
		this.worldRepo = worldsRepository;
	}

	public Object read(Request request, Response response)
	{
		// Get the count of queries to run.
		int count = 1;
		String value = request.getHeader("queries");

		if (value != null)
		{
			count = Integer.parseInt(value);
		}

		// Bounds check.
		if (count > 500)
		{
			count = 500;
		}
		else if (count < 1)
		{
			count = 1;
		}

		// Fetch some rows from the database.
		final World[] worlds = new World[count];
		final Random random = ThreadLocalRandom.current();

		for (int i = 0; i < count; i++)
		{
			worlds[i] = worldRepo.find(random.nextInt(DB_ROWS) + 1);
		}

		if (count == 1)
		{
			return worlds[0];
		}

		return worlds;
	}
}
