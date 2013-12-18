package hello.controllers;

import hello.dao.WorldDao;
import hello.model.World;

import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import ninja.Result;
import ninja.Results;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.google.inject.persist.Transactional;
import ninja.FilterWith;
import ninja.params.Param;

@Singleton
public class HelloDbController {

    private static final int DB_ROWS = 10000;
    private final Random random = ThreadLocalRandom.current();

    @Inject
    WorldDao worldDao;

    @FilterWith(DatabaseAccess.class)
    public Result singleGet() {
	return Results.json().render(getRandomWorld());
    }

    // @Transactional is important here as it encapsulates all
    // JPA calls in dependent methods inside one - and only one - 
    // transaction. Otherwise WorldDao would open x new transactions what
    // is of course slower than only having one encapsulating transaction.
    @FilterWith(DatabaseAccess.class)
    public Result multiGet(@Param("queries") Integer queries) {
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
    
    // @Transactional is important here as it encapsulates all
    // JPA calls in dependent methods inside one - and only one - 
    // transaction. Otherwise WorldDao would open x new transactions what
    // is of course slower than only having one encapsulating transaction.
    @FilterWith(DatabaseAccess.class)
    public Result update(@Param("queries") Integer queries) {
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
        
        // now update stuff:
        for (World world : worlds) {
            world.randomNumber = random.nextInt();
            worldDao.put(world);
        }

	return Results.json().render(worlds);
    }

    private World getRandomWorld() {
	return worldDao.get(random.nextInt(DB_ROWS) + 1);
    }

}
