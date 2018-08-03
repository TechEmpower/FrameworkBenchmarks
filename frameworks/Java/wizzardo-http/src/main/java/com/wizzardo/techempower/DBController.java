package com.wizzardo.techempower;

import com.wizzardo.epoll.ByteBufferProvider;
import com.wizzardo.epoll.ByteBufferWrapper;
import com.wizzardo.http.HttpConnection;
import com.wizzardo.http.framework.Controller;
import com.wizzardo.http.framework.template.Renderer;
import com.wizzardo.http.request.Header;
import com.wizzardo.http.response.Status;
import com.wizzardo.tools.json.JsonTools;

import java.sql.*;
import java.util.Arrays;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class DBController extends Controller {

    DBService dbService;
    static ThreadLocal<ByteBufferProvider> byteBufferProviderThreadLocal = ThreadLocal.<ByteBufferProvider>withInitial(() -> {
        ByteBufferWrapper wrapper = new ByteBufferWrapper(64 * 1024);
        return () -> wrapper;
    });

    static ExecutorService executorService = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

    public Renderer world() throws SQLException {
        World world;
        try (Connection connection = dbService.getConnection();
             PreparedStatement statement = connection.prepareStatement("SELECT id,randomNumber FROM World WHERE id = ?")
        ) {
            statement.setInt(1, getRandomNumber());
            try (ResultSet resultSet = statement.executeQuery()) {
                resultSet.next();
                int id = resultSet.getInt(1);
                int randomNumber = resultSet.getInt(2);
                world = new World(id, randomNumber);
            }
        }

        return renderJson(world);
    }

    public void queries() {
        int queries = Math.min(Math.max(params().getInt("queries", 1), 1), 500);
        response.async();
        AtomicInteger counter = new AtomicInteger(0);
        AtomicBoolean failed = new AtomicBoolean(false);

        World[] worlds = new World[queries];
        for (int i = 0; i < queries; i++) {
            int index = i;

            executorService.submit(() -> {
                try (Connection connection = dbService.getConnection();
                     PreparedStatement statement = connection.prepareStatement("SELECT id,randomNumber FROM World WHERE id = ?")
                ) {
                    statement.setInt(1, getRandomNumber());
                    try (ResultSet resultSet = statement.executeQuery()) {
                        resultSet.next();
                        int id = resultSet.getInt(1);
                        int randomNumber = resultSet.getInt(2);
                        worlds[index] = new World(id, randomNumber);
                    }
                } catch (SQLException e) {
                    if (failed.compareAndSet(false, true)) {
                        response.status(Status._500).body(e.getMessage());
                        commitAsyncResponse();
                    }
                    return;
                }

                if (counter.incrementAndGet() == queries && !failed.get()) {
                    response.appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
                    response.body(JsonTools.serializeToBytes(worlds));

                    commitAsyncResponse();
                }
            });
        }
    }

    public void updates() {
        int queries = Math.min(Math.max(params().getInt("queries", 1), 1), 500);
        response.async();
        AtomicInteger counter = new AtomicInteger(0);
        AtomicBoolean failed = new AtomicBoolean(false);

        World[] worlds = new World[queries];
        for (int i = 0; i < queries; i++) {
            int index = i;
            executorService.submit(() -> {
                try (Connection connection = dbService.getConnection()) {
                    try (PreparedStatement statement = connection.prepareStatement("SELECT id,randomNumber FROM World WHERE id = ?")) {
                        statement.setInt(1, getRandomNumber());
                        try (ResultSet resultSet = statement.executeQuery()) {
                            resultSet.next();
                            int id = resultSet.getInt(1);
                            int randomNumber = resultSet.getInt(2);
                            worlds[index] = new World(id, randomNumber);
                        }
                    }
                } catch (Exception e) {
                    if (failed.compareAndSet(false, true)) {
                        response.status(Status._500).body(e.getMessage());
                        commitAsyncResponse();
                    }
                }


                if (counter.incrementAndGet() == queries && !failed.get()) {
                    executorService.submit(() -> {
                        try (Connection connection = dbService.getConnection()) {
                            try (PreparedStatement statement = connection.prepareStatement("UPDATE World SET randomNumber = ? WHERE id = ?")) {
                                Arrays.sort(worlds, (o1, o2) -> Integer.compare(o2.id, o1.id));
                                for (int j = queries - 1; j >= 0; j--) {
                                    World world = worlds[j];
                                    world.randomNumber = getRandomNumber();
                                    statement.setInt(1, world.randomNumber);
                                    statement.setInt(2, world.id);
                                    if (j > 0)
                                        statement.addBatch();
                                    else
                                        statement.executeBatch();
                                }
                            }

                            response.appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
                            response.body(JsonTools.serializeToBytes(worlds));

                            commitAsyncResponse();
                        } catch (Exception e) {
                            if (failed.compareAndSet(false, true)) {
                                response.status(Status._500).body(e.getMessage());
                                commitAsyncResponse();
                            }
                        }
                    });
                }
            });
        }
    }

    protected void commitAsyncResponse() {
        ByteBufferProvider bufferProvider = byteBufferProviderThreadLocal.get();
        HttpConnection connection = request.connection();
        response.commit(connection, bufferProvider);
        connection.flush(bufferProvider);
        response.reset();
    }

    protected int getRandomNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    public static final class World {
        public int id;
        public int randomNumber;

        public World(int id, int randomNumber) {
            this.id = id;
            this.randomNumber = randomNumber;
        }
    }
}
