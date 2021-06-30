/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.concurrent.*;
import javax.annotation.Resource;
import org.redkale.net.ChannelContext;
import org.redkale.net.http.*;
import org.redkale.service.AbstractService;
import org.redkale.source.*;
import org.redkalex.benchmark.CachedWorld.WorldEntityCache;

/**
 *
 * @author zhangjx
 */
@RestService(name = " ", repair = false)
public class Service extends AbstractService {

    private static final byte[] helloBytes = "Hello, world!".getBytes();

    @Resource
    private DataSource source;

    private WorldEntityCache cache;

    @RestMapping(name = "plaintext")
    public byte[] getHelloBytes() {
        return helloBytes;
    }

    @RestMapping(name = "json")
    public Message getHelloMessage() {
        return Message.create("Hello, World!");
    }

    @RestMapping(name = "db")
    public CompletableFuture<World> findWorldAsync(ChannelContext context) {
        return source.findAsync(World.class, context, 1 + ThreadLocalRandom.current().nextInt(10000));
    }

    @RestMapping(name = "queries")
    public CompletableFuture<World[]> queryWorldAsync(ChannelContext context, int q) {
        final int size = Math.min(500, Math.max(1, q));
        final World[] worlds = new World[size];
        final ThreadLocalRandom random = ThreadLocalRandom.current();
        final CompletableFuture[] futures = new CompletableFuture[size];
        for (int i = 0; i < size; i++) {
            final int index = i;
            futures[index] = source.findAsync(World.class, context, 1 + random.nextInt(10000)).thenAccept(v -> worlds[index] = v);
        }
        return CompletableFuture.allOf(futures).thenApply(v -> worlds);
    }

    @RestMapping(name = "updates")
    public CompletableFuture<World[]> updateWorldAsync(ChannelContext context, int q) {
        final int size = Math.min(500, Math.max(1, q));
        final World[] worlds = new World[size];
        final ThreadLocalRandom random = ThreadLocalRandom.current();
        final CompletableFuture[] futures = new CompletableFuture[size];
        for (int i = 0; i < size; i++) {
            final int index = i;
            futures[index] = source.findAsync(World.class, context, 1 + random.nextInt(10000)).thenAccept(v -> worlds[index] = v.randomNumber(1 + random.nextInt(10000)));
        }
        return CompletableFuture.allOf(futures).thenCompose(v -> source.updateAsync(context, World.sort(worlds))).thenApply(v -> worlds);
    }

    @RestMapping(name = "fortunes")
    public CompletableFuture<HttpResult<String>> queryFortunes() {
        return source.queryListAsync(Fortune.class).thenApply((fortunes) -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            String html = FortunesTemplate.template(Fortune.sort(fortunes)).render().toString();
            return new HttpResult("text/html; charset=utf-8", html);
        });
    }

    @RestMapping(name = "cached-worlds")
    public CachedWorld[] cachedWorlds(int q) {
        if (cache == null) {
            synchronized (this) {
                if (cache == null) cache = new WorldEntityCache(source);
            }
        }
        final int size = Math.min(500, Math.max(1, q));
        final CachedWorld[] worlds = new CachedWorld[size];
        final ThreadLocalRandom random = ThreadLocalRandom.current();
        for (int i = 0; i < size; i++) {
            worlds[i] = cache.findAt(random.nextInt(10000));
        }
        return worlds;
    }

}
