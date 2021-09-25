/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.concurrent.*;
import java.util.function.IntFunction;
import java.util.stream.*;
import javax.annotation.Resource;
import org.redkale.net.http.*;
import org.redkale.service.AbstractService;
import org.redkale.source.*;
import org.redkale.util.Utility;

/**
 *
 * @author zhangjx
 */
@RestService(name = " ", repair = false)
public class BenchmarkService extends AbstractService {

    private static final byte[] helloBytes = "Hello, world!".getBytes();

    @Resource
    private DataSource source;

    private CachedWorld.Cache cache;

    private final IntFunction<World[]> wordArrayFunc = c -> new World[c];

    private final IntFunction<CompletableFuture<World>> wordFindFunc = id -> source.findAsync(World.class, id);

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
        return source.findAsync(World.class, ThreadLocalRandom.current().nextInt(10000) + 1);
    }

    @RestMapping(name = "queries")
    public CompletableFuture<World[]> queryWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        IntStream ids = ThreadLocalRandom.current().ints(size, 1, 10001);
        final Stream<CompletableFuture<World>> futures = ids.mapToObj(wordFindFunc);
        return Utility.allOfFutures(futures, wordArrayFunc);
    }

    @RestMapping(name = "updates")
    public CompletableFuture<World[]> updateWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        IntStream ids = ThreadLocalRandom.current().ints(size, 1, 10001);
        final Stream<CompletableFuture<World>> futures = ids.mapToObj(wordFindFunc);
        final int[] newNumbers = ThreadLocalRandom.current().ints(size, 1, 10001).toArray();
        return Utility.allOfFutures(futures, wordArrayFunc, (i, v) -> v.setRandomNumber(newNumbers[i]))
            .thenCompose(worlds -> source.updateAsync(World.sort(worlds)).thenApply(u -> worlds));
    }

    @RestMapping(name = "fortunes")
    public CompletableFuture<HttpScope> queryFortunes() {
        return source.queryListAsync(Fortune.class).thenApply(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            return HttpScope.refer("").attr("fortunes", Fortune.sort(fortunes));
        });
    }

    @RestMapping(name = "cached-worlds")
    public CachedWorld[] cachedWorlds(int q) {
        if (cache == null) cache = CachedWorld.Cache.instance(source);
        final int size = Math.min(500, Math.max(1, q));
        return cache.random(ThreadLocalRandom.current(), size);
    }
}
