/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.*;
import java.util.concurrent.CompletableFuture;
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

    @Resource
    private CacheSource cache;

    @Override
    public void init(AnyValue conf) {
        try {
            List<CachedWorld> list = source.queryList(CachedWorld.class);
            for (CachedWorld world : list) {
                cache.set(String.valueOf(world.getId()), CachedWorld.class, world);
            }
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @RestMapping(name = "json")
    public Message getHelloMessage() {
        return new Message("Hello, World!");
    }

    @RestMapping(name = "plaintext")
    public byte[] getHelloBytes() {
        return helloBytes;
    }

    @RestMapping(name = "db")
    public World findWorld() {
        return source.find(World.class, randomId());
    }

    @RestMapping(name = "queries")
    public World[] queryWorld(@RestParam(name = "queries") int count) {
        final int size = Math.min(500, Math.max(1, count));
        final CompletableFuture[] futures = new CompletableFuture[size];
        final World[] worlds = new World[size];
        for (int i = 0; i < size; i++) {
            final int index = i;
            futures[index] = source.findAsync(World.class, randomId()).thenApply(r -> worlds[index] = r);
        }
        return CompletableFuture.allOf(futures).thenApply(v -> worlds).join();
    }

    @RestMapping(name = "cached-worlds")
    public CachedWorld[] cachedWorlds(@RestParam(name = "count") int count) {
        final int size = Math.min(500, Math.max(1, count));
        final CachedWorld[] worlds = new CachedWorld[size];
        for (int i = 0; i < size; i++) {
            worlds[i] = (CachedWorld) cache.get(String.valueOf(randomId()), CachedWorld.class);
        }
        return worlds;
    }

    @RestMapping(name = "updates")
    public World[] updateWorld(@RestParam(name = "queries") int count) {
        final int size = Math.min(500, Math.max(1, count));
        final CompletableFuture[] futures = new CompletableFuture[size];
        final World[] worlds = new World[size];
        for (int i = 0; i < size; i++) {
            final int index = i;
            futures[index] = source.findAsync(World.class, randomId()).thenApply((World r) -> {
                r.setRandomNumber(randomId());
                worlds[index] = r;
                return r;
            });
        }
        return CompletableFuture.allOf(futures).thenCompose(v -> source.updateAsync(worlds)).thenApply(v -> worlds).join();
    }

    @RestMapping(name = "fortunes")
    public HttpResult<String> queryFortunes() {
        return source.queryListAsync(Fortune.class).thenApply((fortunes) -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            Collections.sort(fortunes);
            String html = FortunesTemplate.template(fortunes).render().toString();
            return new HttpResult("text/html; charset=UTF-8", html);
        }).join();
    }

    private int randomId() {
        return 1 + random.nextInt(10000);
    }
}
