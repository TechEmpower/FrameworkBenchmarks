package controllers;

import com.google.inject.name.Named;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import models.Fortune;
import models.World;
import play.Play;
import play.libs.F;
import play.libs.Json;
import play.mvc.Controller;
import play.mvc.Result;
import play.mvc.With;
import scala.concurrent.ExecutionContext;
import utils.Headers;
import utils.Predicate;
import utils.Predicated;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.ThreadPoolExecutor;
import javax.inject.Inject;

@With(Headers.class)
public class Application extends Controller {

    private static final int TEST_DATABASE_ROWS = 10000;

    @Inject @Named("dbEc") private ExecutionContext dbEc;

    public static class Message {
        public final String message = "Hello, World!";
    }

    public Result json() {
        return ok(Json.toJson(new Message()));
    }

    // If the thread-pool used by the database grows too large then our server
    // is probably struggling, and we should start dropping requests. Set
    // the max size of our queue something above the number of concurrent
    // connections that we need to handle.
    public static class IsDbAvailable implements Predicate {
        @Inject @Named("dbTpe") private ThreadPoolExecutor tpe;
        @Override
        public boolean condition() {
            return tpe.getQueue().size() <= 1024;
        }
    }

    @Predicated(predicate = IsDbAvailable.class, failed = SERVICE_UNAVAILABLE)
    public F.Promise<Result> db() {
        return getRandomWorlds(1).map(worlds -> ok(Json.toJson(worlds.get(0))));
    }

    @Predicated(predicate = IsDbAvailable.class, failed = SERVICE_UNAVAILABLE)
    public F.Promise<Result> queries(final String queryCountString) {
        return getRandomWorlds(queryCount(queryCountString)).map(worlds -> ok(Json.toJson(worlds)));
    }

    @Predicated(predicate = IsDbAvailable.class, failed = SERVICE_UNAVAILABLE)
    public F.Promise<Result> fortunes() {
        return F.Promise.promise(() -> {
            List<Fortune> fortunes = Fortune.findAll();
            fortunes.add(new Fortune("Additional fortune added at request time."));
            Collections.sort(fortunes, (f1, f2) -> f1.message.compareTo(f2.message));

            return ok(views.html.fortunes.render(fortunes));
        }, dbEc);
    }

    @Predicated(predicate = IsDbAvailable.class, failed = SERVICE_UNAVAILABLE)
    public F.Promise<Result> update(final String queryCountString) {
        return getRandomWorlds(queryCount(queryCountString)).map(worlds -> {
            Random random = ThreadLocalRandom.current();
            for (World world : worlds) {
                world.randomNumber = (long) (random.nextInt(10000) + 1);
            }

            List<World> updatedWorlds = World.save(worlds);
            return ok(Json.toJson(updatedWorlds));
        }, dbEc);
    }

    private int queryCount(String queryCountString) {
        int queryCount;
        try {
            queryCount = Integer.parseInt(queryCountString, 10);
        } catch (NumberFormatException e) {
            queryCount = 1;
        }
        if (queryCount < 1) {
            queryCount = 1;
        } else if (queryCount > 500) {
            queryCount = 500;
        }

        return queryCount;
    }

    private F.Promise<List<World>> getRandomWorlds(final int n) {
        return F.Promise.promise(() -> {
            Random random = ThreadLocalRandom.current();
            List<World> worlds = new ArrayList<>(n);
            for (int i = 0; i < n; ++i) {
                long randomId = random.nextInt(TEST_DATABASE_ROWS) + 1;
                World world = World.findById(randomId);
                worlds.add(world);
            }
            return worlds;
        }, dbEc);
    }

}
