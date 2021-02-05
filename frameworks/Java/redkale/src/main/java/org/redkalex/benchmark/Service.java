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

    @Override
    public void init(AnyValue conf) {
        source.queryList(CachedWorld.class);
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
    public CompletableFuture<World> findWorld() {
        return source.findAsync(World.class, randomId());
    }

    @RestMapping(name = "queries")
    public CompletableFuture<World[]> queryWorld(@RestParam(name = "queries") int count) {
        final int size = Math.min(500, Math.max(1, count));
        final World[] worlds = new World[size];
        final CompletableFuture[] futures = new CompletableFuture[size];
        for (int i = 0; i < size; i++) {
            final int index = i;
            futures[index] = source.findAsync(World.class, randomId()).thenApply(r -> worlds[index] = r);
        }
        return CompletableFuture.allOf(futures).thenApply(v -> worlds);
    }

    @RestMapping(name = "cached-worlds")
    public CachedWorld[] cachedWorlds(@RestParam(name = "count") int count) {
        final int size = Math.min(500, Math.max(1, count));
        final CachedWorld[] worlds = new CachedWorld[size];
        for (int i = 0; i < size; i++) {
            worlds[i] = source.find(CachedWorld.class, randomId());
        }
        return worlds;
    }

    @RestMapping(name = "updates")
    public CompletableFuture<World[]> updateWorld(@RestParam(name = "queries") int count) {
        final int size = Math.min(500, Math.max(1, count));
        final World[] worlds = new World[size];
        final CompletableFuture[] futures = new CompletableFuture[size];
        for (int i = 0; i < size; i++) {
            final int index = i;
            futures[index] = source.findAsync(World.class, randomId()).thenAccept((World r) -> {
                r.setRandomNumber(randomId());
                worlds[index] = r;
            });
        }
        return CompletableFuture.allOf(futures).thenCompose(v -> source.updateAsync(worlds)).thenApply(v -> worlds);
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

    private int randomId() {
        return 1 + random.nextInt(10000);
    }
}
