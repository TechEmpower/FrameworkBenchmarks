package controllers;

import akka.dispatch.ExecutionContexts;
import models.World;
import play.Play;
import play.core.NamedThreadFactory;
import play.libs.F;
import play.libs.Json;

import play.mvc.Controller;
import play.mvc.Result;
import scala.concurrent.ExecutionContext;
import utils.Predicate;
import utils.Predicated;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.*;

public class Application extends Controller {

    private static final int MAX_QUERIES_PER_REQUEST = 20;
    private static final int TEST_DATABASE_ROWS = 10000;

    private static final int partitionCount = Play.application().configuration().getInt("db.default.partitionCount");
    private static final int maxConnections =
            partitionCount * Play.application().configuration().getInt("db.default.maxConnectionsPerPartition");
    private static final int minConnections =
            partitionCount * Play.application().configuration().getInt("db.default.minConnectionsPerPartition");

    private static final ThreadPoolExecutor tpe = new ThreadPoolExecutor(minConnections, maxConnections,
            0L, TimeUnit.MILLISECONDS,
            new LinkedBlockingQueue<Runnable>(),
            new NamedThreadFactory("dbEc"));
    private static final ExecutionContext dbEc = ExecutionContexts.fromExecutorService(tpe);

    // If the thread-pool used by the database grows too large then our server
    // is probably struggling, and we should start dropping requests. Set
    // the max size of our queue something above the number of concurrent
    // connections that we need to handle.
    public static class IsDbAvailable implements Predicate {
        @Override
        public boolean condition() {
            return tpe.getQueue().size() <= 1024;
        }
    }

    @Predicated(predicate = IsDbAvailable.class, failed = SERVICE_UNAVAILABLE)
    public static F.Promise<Result> db() {
        return getRandomWorlds(1).map(new F.Function<List<World>, Result>() {
            @Override
            public Result apply(List<World> worlds) {
                return ok(Json.toJson(worlds.get(0)));
            }
        });
    }

    @Predicated(predicate = IsDbAvailable.class, failed = SERVICE_UNAVAILABLE)
    public static F.Promise<Result> queries(final String queryCountString) {
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

        return getRandomWorlds(queryCount).map(new F.Function<List<World>, Result>() {
            @Override
            public Result apply(List<World> worlds) {
                return ok(Json.toJson(worlds));
            }
        });
    }

    private static F.Promise<List<World>> getRandomWorlds(final int n) {
        return F.Promise.promise(new F.Function0<List<World>>() {
            @Override
            public List<World> apply() {
                Random random = ThreadLocalRandom.current();
                List<World> worlds = new ArrayList<World>(n);
                for (int i = 0; i < n; ++i) {
                    long randomId = random.nextInt(TEST_DATABASE_ROWS) + 1;
                    World world = World.find.byId(randomId);
                    worlds.add(world);
                }
                return worlds;
            }
        }, dbEc);
    }

}
