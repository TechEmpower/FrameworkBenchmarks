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

/**
 *
 * @author zhangjx
 */
@RestService(name = " ", repair = false)
public class Service extends AbstractService {

    private static final byte[] helloBytes = "Hello, world!".getBytes();

    private final ThreadLocal<Random> localRandom = ThreadLocal.withInitial(Random::new);

    @Resource
    private DataSource source;

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
        return source.findAsync(World.class, randomId());
    }

    @RestMapping(name = "queries")
    public CompletableFuture<World[]> queryWorldAsync(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final Random random = localRandom.get();
        final World[] worlds = new World[size];
        
//        final CompletableFuture[] futures = new CompletableFuture[size];
//        for (int i = 0; i < size; i++) {
//            final int index = i;
//            futures[i] = source.findAsync(World.class, randomId(random)).thenAccept(v -> worlds[index] = v);
//        }
//        return CompletableFuture.allOf(futures).thenApply(v -> worlds);
 
        final AtomicInteger index = new AtomicInteger();
        final Function<?, CompletableFuture> func = f -> source.findAsync(World.class, randomId(random))
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
        
//        final CompletableFuture[] futures = new CompletableFuture[size];
//        for (int i = 0; i < size; i++) {
//            final int index = i;
//            futures[i] = source.findAsync(World.class, randomId(random)).thenAccept(v -> worlds[index] = v.randomNumber(randomId(random)));
//        }
//        return CompletableFuture.allOf(futures).thenCompose(v -> source.updateAsync(sort(worlds))).thenApply(v -> worlds);

        final AtomicInteger index = new AtomicInteger();
        final Function<?, CompletableFuture> func = f -> source.findAsync(World.class, randomId(random))
            .thenAccept(v -> worlds[index.getAndIncrement()] = v.randomNumber(randomId(random)));
        CompletableFuture future = func.apply(null);
        for (int i = 1; i < size; i++) {
            future = future.thenCompose(func);
        }
        return future.thenCompose(v -> source.updateAsync(sort(worlds))).thenApply(v -> worlds);
    }

    @RestMapping(name = "cached-worlds")
    public CachedWorld[] cachedWorlds(int q) {
        final int size = Math.min(500, Math.max(1, q));
        final Random random = localRandom.get();
        final CachedWorld[] worlds = new CachedWorld[size];
        for (int i = 0; i < size; i++) {
            worlds[i] = source.find(CachedWorld.class, randomId(random));
        }
        return worlds;
    }

    @RestMapping(name = "fortunes")
    public CompletableFuture<HttpResult<String>> queryFortunes() {
        return source.queryListAsync(Fortune.class).thenApply((fortunes) -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            String html = FortunesTemplate.template(sort(fortunes)).render().toString();
            return new HttpResult("text/html; charset=utf-8", html);
        });
    }

    private World[] sort(World[] words) {
        Arrays.sort(words);
        return words;
    }

    private List<Fortune> sort(List<Fortune> fortunes) {
        Collections.sort(fortunes);
        return fortunes;
    }

    private int randomId() {
        return 1 + localRandom.get().nextInt(10000);
    }

    private int randomId(Random random) {
        return 1 + random.nextInt(10000);
    }
}
