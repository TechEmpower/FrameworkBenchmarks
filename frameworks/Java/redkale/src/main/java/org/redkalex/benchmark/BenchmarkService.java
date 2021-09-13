/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.Random;
import java.util.concurrent.*;
import javax.annotation.Resource;
import org.redkale.net.http.*;
import org.redkale.service.AbstractService;
import org.redkale.source.*;
import org.redkalex.benchmark.CachedWorld.WorldEntityCache;

/**
 *
 * @author zhangjx
 */
@RestService(name = " ", repair = false)
public class BenchmarkService extends AbstractService {

    private static final byte[] helloBytes = "Hello, world!".getBytes();

    @Resource
    private DataSource source;

    @RestMapping(name = "plaintext")
    public byte[] getHelloBytes() {
        return helloBytes;
    }

    @RestMapping(name = "json")
    public Message getHelloMessage() {
        return Message.create("Hello, World!");
    }

    @RestMapping(name = "db")
    public CompletableFuture<World> findWorldAsync() {
        return source.findAsync(World.class, randomId(ThreadLocalRandom.current()));
    }

    @RestMapping(name = "queries")
    public CompletableFuture<World[]> queryWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final Random random = ThreadLocalRandom.current();
        final CompletableFuture<World>[] futures = new CompletableFuture[size];
        for (int i = 0; i < size; i++) {
            futures[i] = source.findAsync(World.class, randomId(random));
        }
        return CompletableFuture.allOf(futures).thenApply(v -> {
            World[] worlds = new World[size];
            for (int i = 0; i < size; i++) {
                worlds[i] = futures[i].join();
            }
            return worlds;
        });
    }

    @RestMapping(name = "updates")
    public CompletableFuture<World[]> updateWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final Random random = ThreadLocalRandom.current();
        final CompletableFuture<World>[] futures = new CompletableFuture[size];
        for (int i = 0; i < size; i++) {
            futures[i] = source.findAsync(World.class, randomId(random));
        }
        return CompletableFuture.allOf(futures).thenCompose(v -> {
            final Random r = ThreadLocalRandom.current();
            final World[] worlds = new World[size];
            for (int i = 0; i < size; i++) {
                worlds[i] = futures[i].join().randomNumber(randomId(r));
            }
            return source.updateAsync(World.sort(worlds)).thenApply(u -> worlds);
        });
    }

    @RestMapping(name = "fortunes")
    public CompletableFuture<HttpScope> queryFortunes() {
        return source.queryListAsync(Fortune.class).thenApply(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            return HttpScope.refer("").attr("fortunes", Fortune.sort(fortunes));
        });
    }

    private WorldEntityCache cache;

    @RestMapping(name = "cached-worlds")
    public CachedWorld[] cachedWorlds(int q) {
        if (cache == null) {
            synchronized (this) {
                if (cache == null) cache = new WorldEntityCache(source);
            }
        }
        final int size = Math.min(500, Math.max(1, q));
        return cache.random(ThreadLocalRandom.current(), size);
    }

    protected int randomId(Random rand) {
        long s = rand.nextLong();
        return (int) ((s < 0 ? -s : s) % 10000) + 1;
    }

}
