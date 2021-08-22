package com.wizzardo.techempower;

import com.wizzardo.epoll.ByteBufferProvider;
import com.wizzardo.epoll.ByteBufferWrapper;
import com.wizzardo.http.HttpConnection;
import com.wizzardo.http.framework.Controller;
import com.wizzardo.http.request.Header;
import com.wizzardo.http.response.Status;
import com.wizzardo.tools.json.JsonTools;
import com.wizzardo.tools.misc.Unchecked;
import io.reactiverse.pgclient.*;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicInteger;

public class DBController extends Controller {

    DBService dbService;
    CachedWorldService cachedWorldService;

    public void world() {
        response.async();
        dbService.getClient().preparedQuery("SELECT * FROM World WHERE id=$1", Tuple.of(getRandomNumber()), res -> {
            if (res.succeeded()) {
                PgIterator resultSet = res.result().iterator();
                if (!resultSet.hasNext()) {
                    response.status(Status._404);
                } else {
                    Tuple row = resultSet.next();
                    response.appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
                    response.body(JsonTools.serializeToBytes(new World(row.getInteger(0), row.getInteger(1))));
                }
            } else {
                res.cause().printStackTrace();
                response.status(Status._500).body(res.cause().getMessage());
            }
            commitAsyncResponse();
        });
    }

    public void queries() {
        int queries = Math.min(Math.max(params().getInt("queries", 1), 1), 500);
        World[] worlds = new World[queries];

        AtomicInteger counter = new AtomicInteger(0);
        AtomicBoolean failed = new AtomicBoolean(false);
        PgPool pool = dbService.getClient();

        response.async();
        for (int i = 0; i < queries; i++) {
            int index = i;
            pool.preparedQuery("SELECT * FROM World WHERE id=$1", Tuple.of(getRandomNumber()), res -> {
                if (res.succeeded()) {
                    PgIterator resultSet = res.result().iterator();
                    Tuple row = resultSet.next();
                    worlds[index] = new World(row.getInteger(0), row.getInteger(1));
                } else {
                    res.cause().printStackTrace();
                    if (failed.compareAndSet(false, true)) {
                        response.status(Status._500).body(res.cause().getMessage());
                        commitAsyncResponse();
                    }
                }

                if (counter.incrementAndGet() == queries && !failed.get()) {
                    response.appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
                    response.body(JsonTools.serializeToBytes(worlds));
                    commitAsyncResponse();
                }
            });
        }
    }

    public void cachedWorlds() {
        int count = Math.min(Math.max(params().getInt("count", 1), 1), 500);
        CachedWorld[] worlds = new CachedWorld[count];

        for (int i = 0; i < count; i++) {
            worlds[i] = cachedWorldService.get(getRandomNumber());
        }

        response.appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
        response.body(JsonTools.serializeToBytes(worlds));
    }

    public void updates() {
        int queries = Math.min(Math.max(params().getInt("queries", 1), 1), 500);
        World[] worlds = new World[queries];

        AtomicInteger counter = new AtomicInteger(0);
        AtomicBoolean failed = new AtomicBoolean(false);
        PgPool pool = dbService.getClient();

        response.async();
        for (int i = 0; i < queries; i++) {
            int index = i;
            pool.preparedQuery("SELECT * FROM World WHERE id=$1", Tuple.of(getRandomNumber()), res -> {
                if (res.succeeded()) {
                    PgIterator resultSet = res.result().iterator();
                    Tuple row = resultSet.next();
                    worlds[index] = new World(row.getInteger(0), row.getInteger(1));
                    worlds[index].randomNumber = getRandomNumber();
                } else {
                    res.cause().printStackTrace();
                    if (failed.compareAndSet(false, true)) {
                        response.status(Status._500).body(res.cause().getMessage());
                        commitAsyncResponse();
                    }
                }

                if (counter.incrementAndGet() == queries && !failed.get()) {
                    Arrays.sort(worlds);
                    List<Tuple> batch = new ArrayList<>(queries);
                    for (World world : worlds) {
                        batch.add(Tuple.of(world.randomNumber, world.id));
                    }

                    pool.preparedBatch("UPDATE world SET randomnumber=$1 WHERE id=$2", batch, ar -> {
                        if (ar.failed()) {
                            response.status(Status._500).body(res.cause().getMessage());
                        } else {
                            response.appendHeader(Header.KV_CONTENT_TYPE_APPLICATION_JSON);
                            response.body(JsonTools.serializeToBytes(worlds));
                        }
                        commitAsyncResponse();
                    });
                }
            });
        }
    }

    public static final class Fortune {
        public final int id;
        public final String message;

        public Fortune(int id, String message) {
            this.id = id;
            this.message = Objects.requireNonNull(message);
        }
    }

    public void fortunes() {
        response.async();
        dbService.getClient().preparedQuery("SELECT * FROM fortune", res -> {
            try {
                if (res.succeeded()) {
                    PgRowSet result = res.result();
                    ArrayList<Fortune> fortunes = new ArrayList<>();
                    for (Row row : result) {
                        fortunes.add(new Fortune(row.getInteger(0), row.getString(1)));
                    }

                    fortunes.add(new Fortune(0, "Additional fortune added at request time."));
                    fortunes.sort(Comparator.comparing(fortune -> fortune.message));

                    model().append("fortunes", fortunes);
                    response.setBody(renderView("fortunes").renderReadableData());
                    response.appendHeader(Header.KV_CONTENT_TYPE_HTML_UTF8);
                } else {
                    res.cause().printStackTrace();
                    response.status(Status._500).body(res.cause().getMessage());
                }

                commitAsyncResponse();
            } catch (Exception e) {
                e.printStackTrace();
            }
        });
    }

    static ThreadLocal<ByteBufferProvider> byteBufferProviderThreadLocal = ThreadLocal.<ByteBufferProvider>withInitial(() -> {
        ByteBufferWrapper wrapper = new ByteBufferWrapper(64 * 1024);
        return () -> wrapper;
    });

    protected void commitAsyncResponse() {
        ByteBufferProvider bufferProvider = byteBufferProviderThreadLocal.get();
        HttpConnection connection = request.connection();
        response.commit(connection, bufferProvider);
        connection.flush(bufferProvider);
        response.reset();
        Unchecked.run(connection::onFinishingHandling);
    }

    protected int getRandomNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

}
