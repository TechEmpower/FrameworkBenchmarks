package hello.controllers;

import hello.dao.WorldDao;
import hello.model.World;

import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import ninja.Result;
import ninja.Results;
import ninja.params.PathParam;

import com.google.inject.Inject;
import com.google.inject.Singleton;

@Singleton
public class HelloDbController {

    private static final int DB_ROWS = 10000;
    private final Random random = ThreadLocalRandom.current();

    @Inject
    WorldDao worldDao;

    public Result singleGet() {
	return Results.json().render(getRandomWorld());
    }

    public Result multiGet(@PathParam("queries") Integer queries) {
	if (queries == null || queries < 1) {
	    queries = 1;
	}
	if (queries > 500) {
	    queries = 500;
	}

	final World[] worlds = new World[queries];

	for (int i = 0; i < queries; i++) {
	    worlds[i] = getRandomWorld();
	}

	return Results.json().render(worlds);
    }

    private World getRandomWorld() {
	return worldDao.get(random.nextInt(DB_ROWS) + 1);
    }

}
