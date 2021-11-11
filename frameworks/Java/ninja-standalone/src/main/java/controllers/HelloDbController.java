package controllers;

import dao.WorldDao;
import model.World;

import java.util.concurrent.ThreadLocalRandom;

import ninja.Result;
import ninja.Results;

import com.google.inject.Inject;
import com.google.inject.Singleton;
import com.google.inject.persist.Transactional;
import ninja.jpa.UnitOfWork;
import ninja.params.Param;
import java.util.concurrent.atomic.AtomicInteger;

@Singleton
public class HelloDbController {

    private static final int DB_ROWS = 10000;

    @Inject
    WorldDao worldDao;

    @UnitOfWork
    public Result singleGet() {
    	//Cache control header is set to disable the double setting of the date header.
        return Results.json().render(getRandomWorld(ThreadLocalRandom.current().nextInt(DB_ROWS) + 1)).addHeader(Result.CACHE_CONTROL, "");
    }

    @UnitOfWork
    public Result multiGet(@Param("queries") Integer queries) {
        if (queries == null || queries < 1) {
            queries = 1;
        }
        if (queries > 500) {
            queries = 500;
        }

        final World[] worlds = new World[queries];

        //Pick unique random numbers 
        final AtomicInteger i = new AtomicInteger(0);
        ThreadLocalRandom.current().ints(1, DB_ROWS).distinct().limit(queries).forEach(
            (randomValue)->worlds[i.getAndAdd(1)] = getRandomWorld(randomValue)
        );

        //Cache control header is set to disable the double setting of the date header.
        return Results.json().render(worlds).addHeader(Result.CACHE_CONTROL, "");
    }

    @UnitOfWork
    public Result update(@Param("queries") Integer queries) {
        if (queries == null || queries < 1) {
            queries = 1;
        }
        if (queries > 500) {
            queries = 500;
        }

        final World[] worlds = new World[queries];

        //Pick unique random numbers 
        final AtomicInteger i = new AtomicInteger(0);
        ThreadLocalRandom.current().ints(1, DB_ROWS).distinct().limit(queries).forEach(
            (randomValue)->worlds[i.getAndAdd(1)] = getRandomWorld(randomValue)
        );

        // now update stuff:
        for (World world : worlds) {
            world.randomNumber = ThreadLocalRandom.current().nextInt(DB_ROWS) + 1;
            this.updateWorld(world);
        }
        //Cache control header is set to disable the double setting of the date header.
        return Results.json().render(worlds).addHeader(Result.CACHE_CONTROL, "");
    }

    @Transactional
    public void updateWorld(World world) {
        worldDao.put(world);
    }

    private World getRandomWorld(int i) {
        return worldDao.get(i);
    }

}
