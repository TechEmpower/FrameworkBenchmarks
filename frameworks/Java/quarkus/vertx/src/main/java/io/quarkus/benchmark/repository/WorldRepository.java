package io.quarkus.benchmark.repository;

import io.quarkus.benchmark.model.World;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.json.JsonArray;
import io.vertx.core.json.JsonObject;
import io.vertx.sqlclient.PreparedQuery;
import io.vertx.sqlclient.Row;
import io.vertx.sqlclient.RowIterator;
import io.vertx.sqlclient.RowSet;
import io.vertx.sqlclient.Tuple;
import jakarta.inject.Inject;
import jakarta.inject.Singleton;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

@Singleton
public class WorldRepository {

    @Inject
    private PgConnectionPool connectionPool;

    private static final Integer[] BOXED_RND = IntStream.range(1, 10001).boxed().toArray(Integer[]::new);
    private static final Tuple[] tupleCache = new Tuple[10000];

    static {
        for (int i = 0; i < 10000; i++) {
            tupleCache[i] = Tuple.of(i + 1);
        }
    }


    private static Integer boxedRandomWorldNumber() {
        final int rndValue = ThreadLocalRandom.current().nextInt(1, 10001);
        final var boxedRnd = BOXED_RND[rndValue - 1];
        assert boxedRnd.intValue() == rndValue;
        return boxedRnd;
    }

    private static int primitiveRandomWorldNumber() {
        final int rndValue = ThreadLocalRandom.current().nextInt(1, 10001);
        return rndValue;
    }

    private static Tuple getRandomTuple() {
        final int rndValue = primitiveRandomWorldNumber();
        final Tuple tuple = tupleCache[rndValue - 1];
        return tuple;
    }


    public void loadRandomJsonWorld(final Handler<AsyncResult<JsonWorld>> worldHandler) {
        connectionPool.pgConnection().selectWorldQuery().execute(getRandomTuple(), randomWorldRow -> {
            if (randomWorldRow.succeeded()) {
                final RowIterator<Row> resultSet = randomWorldRow.result().iterator();
                if (!resultSet.hasNext()) {
                    worldHandler.handle(Future.succeededFuture());
                    return;
                }
                final Row row = resultSet.next();
                worldHandler.handle(Future.succeededFuture(new JsonWorld(row.getInteger(0), row.getInteger(1))));
            } else {
                worldHandler.handle(Future.failedFuture(randomWorldRow.cause()));
            }
        });
    }

    public void loadNJsonWorlds(final int count, final Handler<AsyncResult<JsonWorlds>> worldsHandler) {
        FindRandomWorldsCommand.execute(connectionPool, count, worldsHandler);
    }

    public void updateNJsonWorlds(final int count, final Handler<AsyncResult<JsonWorlds>> worldsHandler) {
        UpdateWorldsCommand.execute(connectionPool, count, worldsHandler);
    }

    public static class JsonWorlds extends JsonArray {
        private JsonWorlds(final int capacity) {
            super(new ArrayList(capacity));
        }

        private JsonWorlds(final World[] worlds) {
            this(worlds.length);
            for (final World world : worlds) {
                add(new JsonWorld(world.getId(), world.getRandomNumber()));
            }
        }
    }

    public static class JsonWorld extends JsonObject {
        private JsonWorld(final Integer id, final Integer random) {
            super(Map.of("id", id, "randomNumber", random));
        }
    }

    private static final class UpdateWorldsCommand {
        private final PgConnectionPool.PgClientConnection connection;
        private final World[] worldsToUpdate;
        private final Handler<AsyncResult<JsonWorlds>> resultHandler;
        private boolean failed;
        private int selectWorldCompletedCount;

        private UpdateWorldsCommand(final PgConnectionPool.PgClientConnection connection, final int queries, final Handler<AsyncResult<JsonWorlds>> resultHandler) {
            this.connection = connection;
            this.worldsToUpdate = new World[queries];
            this.resultHandler = resultHandler;
        }

        // execute
        public static void execute(final PgConnectionPool connectionPool, final int count, final Handler<AsyncResult<JsonWorlds>> resultHandler) {
            new UpdateWorldsCommand(connectionPool.pgConnection(), count, resultHandler).run();
        }

        private void run() {
            connection.rawConnection().group(c -> {
                final PreparedQuery<RowSet<Row>> preparedQuery = c.preparedQuery(PgConnectionPool.SELECT_WORLD);
                for (int i = 0; i < worldsToUpdate.length; i++) {
                    final int index = i;
                    preparedQuery.execute(getRandomTuple(), worldId -> {
                        if (!failed) {
                            if (worldId.failed()) {
                                failed = true;
                                resultHandler.handle(Future.failedFuture(worldId.cause()));
                                return;
                            }
                            worldsToUpdate[index] = new World(worldId.result().iterator().next().getInteger(0), primitiveRandomWorldNumber());
                            if (++selectWorldCompletedCount == worldsToUpdate.length) {
                                randomWorldsQueryCompleted();
                            }
                        }
                    });
                }
            });
        }

        private void randomWorldsQueryCompleted() {
            Arrays.sort(worldsToUpdate);
            final List<Integer> params = new ArrayList<>(worldsToUpdate.length * 2);
            for (int i = 0, count = worldsToUpdate.length; i < count; i++) {
                var world = worldsToUpdate[i];
                params.add(world.getId());
                params.add(world.getRandomNumber());
            }
            connection.updateWorldQuery(worldsToUpdate.length).execute(Tuple.wrap(params), updateResult -> {
                if (updateResult.failed()) {
                    resultHandler.handle(Future.failedFuture(updateResult.cause()));
                    return;
                }
                resultHandler.handle(Future.succeededFuture(new JsonWorlds(worldsToUpdate)));
            });
        }
    }

    private static final class FindRandomWorldsCommand implements Handler<AsyncResult<RowSet<Row>>> {
        private final Handler<AsyncResult<JsonWorlds>> resultHandler;
        private final int count;
        private final JsonWorlds jsonWorlds;
        private final PgConnectionPool.PgClientConnection connection;
        private boolean failed;

        private FindRandomWorldsCommand(final PgConnectionPool.PgClientConnection connection, final int count, final Handler<AsyncResult<JsonWorlds>> resultHandler) {
            this.connection = connection;
            this.count = count;
            this.jsonWorlds = new JsonWorlds(count);
            this.resultHandler = resultHandler;
        }

        public static void execute(final PgConnectionPool connectionPool, final int queries, final Handler<AsyncResult<JsonWorlds>> resultHandler) {
            new FindRandomWorldsCommand(connectionPool.pgConnection(), queries, resultHandler).run();
        }

        private void run() {
            connection.rawConnection().group(c -> {
                for (int i = 0; i < count; i++) {
                    c.preparedQuery(PgConnectionPool.SELECT_WORLD).execute(getRandomTuple(), this);
                }
            });
        }

        @Override
        public void handle(final AsyncResult<RowSet<Row>> ar) {
            if (!failed) {
                if (ar.failed()) {
                    failed = true;
                    resultHandler.handle(Future.failedFuture(ar.cause()));
                    return;
                }

                final Tuple row = ar.result().iterator().next();
                jsonWorlds.add(new JsonWorld(row.getInteger(0), row.getInteger(1)));

                // stop condition
                if (jsonWorlds.size() == count) {
                    resultHandler.handle(Future.succeededFuture(jsonWorlds));
                }
            }
        }
    }

}
