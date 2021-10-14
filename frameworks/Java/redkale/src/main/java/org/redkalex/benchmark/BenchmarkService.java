/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.concurrent.*;
import java.util.stream.*;
import javax.annotation.Resource;
import org.redkale.net.http.*;
import org.redkale.service.AbstractService;
import org.redkale.source.*;
import org.redkale.util.AnyValue;

/**
 *
 * @author zhangjx
 */
@RestService(name = " ", repair = false)
public class BenchmarkService extends AbstractService {

    private static final byte[] helloBytes = "Hello, world!".getBytes();

    @Resource
    private DataSource source;

    @Override
    public void init(AnyValue conf) {
        CachedWorld.Cache.getInstance(source);
    }

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
        return source.findsAsync(World.class, ids.boxed());
    }

    @RestMapping(name = "updates")
    public CompletableFuture<World[]> updateWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        IntStream ids = ThreadLocalRandom.current().ints(size, 1, 10001);
        final int[] newNumbers = ThreadLocalRandom.current().ints(size, 1, 10001).toArray();
        return source.findsAsync(World.class, ids.boxed()).thenCompose(words -> source.updateAsync(World.setNewNumbers(words, newNumbers)).thenApply(v -> words));
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
        final int size = Math.min(500, Math.max(1, q));
        return CachedWorld.Cache.getInstance(source).random(ThreadLocalRandom.current(), size);
    }
}
