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

    private static final byte[] helloBytes = "Hello, world!".intern().getBytes();

    private final ThreadLocal<Random> localRandom = ThreadLocal.withInitial(Random::new);

    @Resource
    private DataSource source;

    private EntityCache<CachedWorld> entityCache;

    @Override
    public void init(AnyValue conf) {
        if (source instanceof DataSqlSource) {
            this.entityCache = ((DataSqlSource) source).loadCache(CachedWorld.class);
        }
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
        return source.findAsync(World.class, 1 + localRandom.get().nextInt(10000));
    }

    @RestMapping(name = "queries")
    public CompletableFuture<World[]> queryWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final Random random = localRandom.get();
        final World[] worlds = new World[size];

        final AtomicInteger index = new AtomicInteger();
        final Function<?, CompletableFuture> func = f -> source.findAsync(World.class, 1 + random.nextInt(10000))
            .thenAccept(v -> worlds[index.getAndIncrement()] = v);
        CompletableFuture future = func.apply(null);
        for (int i = 1; i < size; i++) {
            future = future.thenCompose(func);
        }
        return future.thenApply(v -> worlds);
    }

    @RestMapping(name = "updates")
    public CompletableFuture<World[]> updateWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final Random random = localRandom.get();
        final World[] worlds = new World[size];

        final AtomicInteger index = new AtomicInteger();
        final Function<?, CompletableFuture> func = f -> source.findAsync(World.class, 1 + random.nextInt(10000))
            .thenAccept(v -> worlds[index.getAndIncrement()] = v.randomNumber(1 + random.nextInt(10000)));
        CompletableFuture future = func.apply(null);
        for (int i = 1; i < size; i++) {
            future = future.thenCompose(func);
        }
        return future.thenCompose(v -> {
            Arrays.sort(worlds);
            return source.updateAsync(worlds);
        }).thenApply(v -> worlds);
    }

    @RestMapping(name = "fortunes")
    public CompletableFuture<HttpResult<String>> queryFortunes() {
        return source.queryListAsync(Fortune.class).thenApply((fortunes) -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            Collections.sort(fortunes);
            String html = FortunesTemplate.template(fortunes).render().toString();
            return new HttpResult("text/html; charset=utf-8", html);
        });
    }

    @RestMapping(name = "cached-worlds")
    public CachedWorld[] cachedWorlds(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final Random random = localRandom.get();
        final CachedWorld[] worlds = new CachedWorld[size];
        for (int i = 0; i < size; i++) {
            worlds[i] = entityCache.find(1 + random.nextInt(10000));
        }
        return worlds;
    }

}
