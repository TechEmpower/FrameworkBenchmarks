package controllers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import models.Fortune;
import models.World;
import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Result;

public class Application extends Controller {

    public Result db() {
        return ok(Json.toJson(getRandomWorlds(1).get(0)));
    }

    public Result queries(final String queries) {
        return ok(Json.toJson(getRandomWorlds(queryCount(queries))));
    }

    public Result fortunes() {
        final List<Fortune> fortunes = Fortune.findAll();
        fortunes.add(new Fortune("Additional fortune added at request time."));
        Collections.sort(fortunes, (f1, f2) -> f1.message.compareTo(f2.message));

        return ok(views.html.fortunes.render(fortunes));
    }

    public Result update(final String queries) {
        final List<World> worlds = getRandomWorlds(queryCount(queries));
        final Random random = ThreadLocalRandom.current();
        for (final World world : worlds) {
            world.randomNumber = (long) (random.nextInt(10000) + 1);
        }

        final List<World> updatedWorlds = World.save(worlds);
        return ok(Json.toJson(updatedWorlds));
    }

    private int queryCount(final String queryCountString) {
        int queryCount;
        try {
            queryCount = Integer.parseInt(queryCountString, 10);
        } catch (final NumberFormatException e) {
            queryCount = 1;
        }
        if (queryCount < 1) {
            queryCount = 1;
        } else if (queryCount > 500) {
            queryCount = 500;
        }

        return queryCount;
    }

    private List<World> getRandomWorlds(final int n) {
        final Random random = ThreadLocalRandom.current();
        final List<World> worlds = new ArrayList<>(n);
        for (int i = 0; i < n; ++i) {
            long randomId = random.nextInt(10000) + 1;
            final World world = World.find(randomId);
            worlds.add(world);
        }
        return worlds;
    }

}
