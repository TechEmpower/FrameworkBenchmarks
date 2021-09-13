/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import com.fizzed.rocker.RockerOutput;
import com.fizzed.rocker.runtime.ArrayOfByteArraysOutput;
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

    private final ThreadLocal<RedRandom> rands = ThreadLocal.withInitial(RedRandom::new);

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
        return source.findAsync(World.class, randomId(rands.get()));
    }

    @RestMapping(name = "queries")
    public CompletableFuture<World[]> queryWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final Random random = rands.get();
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
        final Random random = rands.get();
        final CompletableFuture<World>[] futures = new CompletableFuture[size];
        for (int i = 0; i < size; i++) {
            futures[i] = source.findAsync(World.class, randomId(random));
        }
        return CompletableFuture.allOf(futures).thenCompose(v -> {
            final Random r = rands.get();
            final World[] worlds = new World[size];
            for (int i = 0; i < size; i++) {
                worlds[i] = futures[i].join().randomNumber(randomId(r));
            }
            return source.updateAsync(World.sort(worlds)).thenApply(u -> worlds);
        });
    }

    @RestMapping(name = "fortunes")
    public CompletableFuture<HttpResult<byte[]>> queryFortunes() {
        return source.queryListAsync(Fortune.class).thenApply(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            RockerOutput out = FortunesTemplate.template(Fortune.sort(fortunes)).render();
            byte[] bs = ((ArrayOfByteArraysOutput) out).toByteArray();
            return new HttpResult("text/html; charset=utf-8", bs);
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
        return cache.random(rands.get(), size);
    }

    protected int randomId(Random rand) {
        long s = rand.nextLong();
        return (int) ((s < 0 ? -s : s) % 10000) + 1;
    }

}
