package com.networknt.techempower;

import com.google.common.net.MediaType;
import com.networknt.techempower.model.World;
import io.undertow.server.HttpServerExchange;

import javax.sql.DataSource;
import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.util.Deque;
import java.util.List;
import java.util.concurrent.*;
import java.util.stream.Collectors;

/**
 * Provides utility methods for the benchmark tests.
 */
public final class Helper {

    private Helper() {
        throw new AssertionError();
    }

    /**
     * Returns the value of the "queries" request parameter, which is an integer
     * bound between 1 and 500 with a default value of 1.
     *
     * @param exchange the current HTTP exchange
     * @return the value of the "queries" request parameter
     */
    public static int getQueries(HttpServerExchange exchange) {
        Deque<String> values = exchange.getQueryParameters().get("queries");
        if (values == null) {
            return 1;
        }
        String textValue = values.peekFirst();
        if (textValue == null) {
            return 1;
        }
        try {
            int parsedValue = Integer.parseInt(textValue);
            return Math.min(500, Math.max(1, parsedValue));
        } catch (NumberFormatException e) {
            return 1;
        }
    }

    /**
     * Returns a random integer that is a suitable value for both the {@code id}
     * and {@code randomNumber} properties of a world object.
     *
     * @return a random world number
     */
    public static int randomWorld() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    private static final int cpuCount = Runtime.getRuntime().availableProcessors();

    public static final Executor executor =
            Executors.newFixedThreadPool(2000,
                    new ThreadFactory() {
                        @Override
                        public Thread newThread(Runnable r) {
                            Thread t = new Thread(r);
                            t.setDaemon(true);
                            return t;
                        }
                    });

    public static World selectWorld(DataSource ds) {
        try (final Connection connection = ds.getConnection()) {
            try (PreparedStatement statement = connection.prepareStatement(
                    "SELECT * FROM world WHERE id = ?",
                    ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {
                statement.setInt(1, Helper.randomWorld());
                try (ResultSet resultSet = statement.executeQuery()) {
                    resultSet.next();
                    return new World(
                            resultSet.getInt("id"),
                            resultSet.getInt("randomNumber"));
                }
            }
        } catch (Exception e) {
            return null;
        }
    }

    public static World updateWorld(DataSource ds) {
        World world;
        try (final Connection connection = ds.getConnection()) {
            try (PreparedStatement update = connection.prepareStatement(
                    "UPDATE world SET randomNumber = ? WHERE id= ?")) {
                try (PreparedStatement query = connection.prepareStatement(
                        "SELECT * FROM world WHERE id = ?",
                        ResultSet.TYPE_FORWARD_ONLY, ResultSet.CONCUR_READ_ONLY)) {

                    query.setInt(1, Helper.randomWorld());
                    try (ResultSet resultSet = query.executeQuery()) {
                        resultSet.next();
                        world = new World(
                                resultSet.getInt("id"),
                                resultSet.getInt("randomNumber"));
                    }
                }
                world.randomNumber = Helper.randomWorld();
                update.setInt(1, world.randomNumber);
                update.setInt(2, world.id);
                update.executeUpdate();
                return world;
            }
        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

    public static <T> CompletableFuture<List<T>> sequence(List<CompletableFuture<T>> futures) {
        CompletableFuture<Void> allDoneFuture =
                CompletableFuture.allOf(futures.toArray(new CompletableFuture[futures.size()]));
        return allDoneFuture.thenApply(v ->
                futures.stream().
                        map(future -> future.join()).
                        collect(Collectors.<T>toList())
        );
    }

}
