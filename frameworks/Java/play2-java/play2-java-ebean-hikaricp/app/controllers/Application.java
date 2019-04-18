package controllers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.CompletionStage;
import java.util.concurrent.ThreadLocalRandom;

import javax.inject.Inject;

import models.Fortune;
import models.World;
import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Result;
import utils.DatabaseExecutionContext;

public class Application extends Controller {

    private final DatabaseExecutionContext dbEc;

    @Inject
    public Application(final DatabaseExecutionContext dbEc) {
        this.dbEc = dbEc;
    }

    public CompletionStage<Result> db() {
        return getRandomWorlds(1).thenApply(worlds -> ok(Json.toJson(worlds.get(0))));
    }

    public CompletionStage<Result> queries(final String queries) {
        return getRandomWorlds(queryCount(queries)).thenApply(worlds -> ok(Json.toJson(worlds)));
    }

    public CompletionStage<Result> fortunes() {
        return CompletableFuture.supplyAsync(() -> {
            final List<Fortune> fortunes = Fortune.findAll();
            fortunes.add(new Fortune("Additional fortune added at request time."));
            Collections.sort(fortunes, (f1, f2) -> f1.message.compareTo(f2.message));

            return ok(views.html.fortunes.render(fortunes));
        }, dbEc);
    }

    public CompletionStage<Result> update(final String queries) {
        return getRandomWorlds(queryCount(queries)).thenApplyAsync(worlds -> {
            final Random random = ThreadLocalRandom.current();
            for (final World world : worlds) {
                world.randomNumber = (long) (random.nextInt(10000) + 1);
            }

            final List<World> updatedWorlds = World.save(worlds);
            return ok(Json.toJson(updatedWorlds));
        }, dbEc);
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

    private CompletionStage<List<World>> getRandomWorlds(final int n) {
        return CompletableFuture.supplyAsync(() -> {
            final Random random = ThreadLocalRandom.current();
            final List<World> worlds = new ArrayList<>(n);
            for (int i = 0; i < n; ++i) {
                long randomId = random.nextInt(10000) + 1;
                final World world = World.find(randomId);
                worlds.add(world);
            }
            return worlds;
        }, dbEc);
    }

}
