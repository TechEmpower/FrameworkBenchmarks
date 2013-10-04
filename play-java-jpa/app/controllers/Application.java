package controllers;

import akka.dispatch.ExecutionContexts;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
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
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

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

    // A predicate for checking our ability to service database requests is determined by ensuring that the request
    // queue doesn't fill up beyond a certain threshold. For convenience we use the max number of connections * the max
    // # of db requests per web request to determine this threshold. It is a rough check as we don't know how many
    // queries we're going to make or what other threads are running in parallel etc. Nevertheless, the check is
    // adequate in order to throttle the acceptance of requests to the size of the pool.
    public static class IsDbAvailable implements Predicate {
        @Override
        public boolean condition() {
            return (tpe.getQueue().size() < maxConnections * MAX_QUERIES_PER_REQUEST);
        }
    }

    public static Result json() {
        final ObjectNode result = OBJECT_MAPPER.createObjectNode();
        result.put("message", "Hello World!");
        return ok(result);
    }


    @Predicated(predicate = IsDbAvailable.class, failed = SERVICE_UNAVAILABLE)
    public static F.Promise<Result> db(final Integer queries) {
        final Random random = ThreadLocalRandom.current();
        final List<F.Promise<? extends World>> promises = new ArrayList<F.Promise<? extends World>>(queries);
        for (int i = 0; i < queries; ++i) {
            final F.Promise<World> p = F.Promise.promise(new F.Function0<World>() {
                @Override
                public World apply() throws Throwable {
                    return World.findById(Long.valueOf(random.nextInt(TEST_DATABASE_ROWS) + 1));
                }
            }, dbEc);
            promises.add(p);
        }
        return F.Promise.sequence(promises).map(new F.Function<List<World>, Result>() {
            @Override
            public Result apply(List<World> worlds) {
                return ok(Json.toJson(worlds));
            }
        });
    }

}
