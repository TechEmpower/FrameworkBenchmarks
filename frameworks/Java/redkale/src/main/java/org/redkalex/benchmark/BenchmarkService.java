/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.*;
import java.util.concurrent.*;
import java.util.function.IntFunction;
import java.util.stream.IntStream;
import org.redkale.annotation.*;
import org.redkale.net.http.*;
import org.redkale.service.AbstractService;
import org.redkale.source.DataSource;
import org.redkale.util.AnyValue;

/**
 *
 * @author zhangjx
 */
@NonBlocking
@RestService(name = " ", repair = false)
public class BenchmarkService extends AbstractService {

    private static final byte[] helloBytes = "Hello, world!".getBytes();

    @Resource
    private DataSource source;

    private WorldCache cache;

    @Override
    public void init(AnyValue conf) {
        this.cache = new WorldCache(source);
    }

    @RestMapping(name = "plaintext")
    public byte[] getHelloBytes() {
        return helloBytes;
    }

    @RestMapping(name = "json")
    public Message getHelloMessage() {
        return new Message("Hello, World!");
    }

    @RestMapping(name = "db")
    public CompletableFuture<World> findWorldAsync() {
        return source.findAsync(World.class, ThreadLocalRandom.current().nextInt(10000) + 1);
    }

    @RestMapping(name = "queries")
    public CompletableFuture<List<World>> queryWorldAsync(int q) {
        int size = Math.min(500, Math.max(1, q));
        IntStream ids = ThreadLocalRandom.current().ints(size, 1, 10001);
        return source.findsListAsync(World.class, ids.boxed());
    }

    @RestMapping(name = "updates")
    public CompletableFuture<List<World>> updateWorldAsync(int q) {
        int size = Math.min(500, Math.max(1, q));
        IntStream ids = ThreadLocalRandom.current().ints(size, 1, 10001);
        int[] newNumbers = ThreadLocalRandom.current().ints(size, 1, 10001).toArray();
        return source.findsListAsync(World.class, ids.boxed())
            .thenCompose(words -> source.updateAsync(World.updateNewNumbers(words, newNumbers))
            .thenApply(v -> words));
    }

    @RestMapping(name = "fortunes")
    public CompletableFuture<HttpScope> queryFortunes() {
        return source.queryListAsync(Fortune.class).thenApply(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            Collections.sort(fortunes);
            return HttpScope.refer("").referObj(fortunes);
        });
    }

    @RestMapping(name = "cached-worlds")
    public World[] cachedWorlds(int q) {
        int size = Math.min(500, Math.max(1, q));
        return cache.random(size);
    }

    static class WorldCache {

        private final IntFunction<World[]> arrayFunc = c -> new World[c];

        private final World[] array;

        private final IntFunction<World> mapFunc;

        public WorldCache(DataSource source) {
            List<World> list = source.queryList(World.class);
            this.array = list.toArray(new World[list.size()]);
            this.mapFunc = c -> array[c];
        }

        public World[] random(int size) {
            IntStream ids = ThreadLocalRandom.current().ints(size, 0, 10000);
            return ids.mapToObj(mapFunc).toArray(arrayFunc);
        }
    }
}
