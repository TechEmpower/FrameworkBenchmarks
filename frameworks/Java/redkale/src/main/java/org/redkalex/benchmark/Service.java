/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
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
public class Service extends AbstractService {

    private static final byte[] helloBytes = "Hello, world!".getBytes();

    private final Random random = new Random();

    @Resource
    private DataSource source;


    @RestMapping(name = "json")
    public Message getHelloMessage() {
        return new Message("Hello, World!");
    }

    @RestMapping(name = "plaintext")
    public byte[] getHelloBytes() {
        return helloBytes;
    }

    @RestMapping(name = "cached-worlds")
    public CachedWorld[] cachedWorlds(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final CachedWorld[] worlds = new CachedWorld[size];
        for (int i = 0; i < size; i++) {
            worlds[i] = source.find(CachedWorld.class, randomId());
        }
        return worlds;
    }

    @RestMapping(name = "db")
    public CompletableFuture<World> findWorldAsync() {
        return source.findAsync(World.class, randomId());
    }

    @RestMapping(name = "fortunes")
    public CompletableFuture<HttpResult<String>> queryFortunes() {
        return source.queryListAsync(Fortune.class).thenApply((fortunes) -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            Collections.sort(fortunes);
            String html = FortunesTemplate.template(fortunes).render().toString();
            return new HttpResult("text/html; charset=UTF-8", html);
        });
    }

    @RestMapping(name = "queries")
    public CompletableFuture<World[]> queryWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final World[] worlds = new World[size];
        final AtomicInteger index = new AtomicInteger();
        final Function<?, CompletableFuture> func = f -> source.findAsync(World.class, randomId())
            .thenAccept((World v) -> worlds[index.getAndIncrement()] = v);
        CompletableFuture future = func.apply(null);
        for (int i = 1; i < size; i++) {
            future = future.thenCompose(func);
        }
        return future.thenApply(v -> worlds);
    }

    @RestMapping(name = "updates")
    public CompletableFuture<World[]> updateWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final World[] worlds = new World[size];
        final AtomicInteger index = new AtomicInteger();
        final Function<?, CompletableFuture> func = f -> source.findAsync(World.class, randomId())
            .thenApply((World v) -> {
                worlds[index.getAndIncrement()] = v;
                v.setRandomNumber(randomId());
                return v;
            });
        CompletableFuture future = func.apply(null);
        for (int i = 1; i < size; i++) {
            future = future.thenCompose(func);
        }
        return future.thenCompose(v -> {
            Arrays.sort(worlds);
            return source.updateAsync(worlds);
        }).thenApply(v -> worlds);
    }

    private int randomId() {
        return 1 + random.nextInt(10000);
    }
}
