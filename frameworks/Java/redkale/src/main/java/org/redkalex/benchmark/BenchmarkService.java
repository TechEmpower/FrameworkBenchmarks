/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.IntStream;
import org.redkale.annotation.*;
import org.redkale.net.http.*;
import org.redkale.service.AbstractService;
import org.redkale.source.DataSource;
import org.redkale.util.*;

/**
 *
 * @author zhangjx
 */
@NonBlocking
@RestService(name = " ", repair = false)
public class BenchmarkService extends AbstractService {

    private static final byte[] helloBytes = "Hello, world!".getBytes();

    @Resource
    private DataSource source;

    @Override
    public void init(AnyValue conf){
        World w = new World();
        testFieldName(Fortune::getMessage); 
        testFieldName(World::getId); 
        testFieldName(w::getId); 
    }
    
    private static <T> void testFieldName(LambdaFunction<T, ?> func) {
        System.out.println("" + LambdaFunction.readColumn(func));
    }

    private static void testFieldName(LambdaSupplier func) {
        System.out.println("" + LambdaSupplier.readColumn(func));
    }
    
    @RestMapping(auth = false)
    public byte[] plaintext() {
        return helloBytes;
    }

    @RestMapping(auth = false)
    public Message json() {
        return new Message("Hello, World!");
    }

    @RestMapping(auth = false)
    public CompletableFuture<World> db() {
        return source.findAsync(World.class, ThreadLocalRandom.current().nextInt(10000) + 1);
    }

    @RestMapping(auth = false)
    public CompletableFuture<List<World>> queries(int q) {
        int size = Math.min(500, Math.max(1, q));
        IntStream ids = ThreadLocalRandom.current().ints(size, 1, 10001);
        return source.findsListAsync(World.class, ids.boxed());
    }

    @RestMapping(auth = false)
    public CompletableFuture<List<World>> updates(int q) {
        int size = Math.min(500, Math.max(1, q));
        IntStream ids = ThreadLocalRandom.current().ints(size, 1, 10001);
        int[] newNumbers = ThreadLocalRandom.current().ints(size, 1, 10001).toArray();
        return source.findsListAsync(World.class, ids.boxed())
            .thenCompose(words -> source.updateAsync(World.updateNewNumbers(words, newNumbers))
            .thenApply(v -> words));
    }

    @RestMapping(auth = false)
    public CompletableFuture<HttpScope> fortunes() {
        return source.queryListAsync(Fortune.class).thenApply(fortunes -> {
            fortunes.add(new Fortune(0, "Additional fortune added at request time."));
            Collections.sort(fortunes);
            return HttpScope.refer("").referObj(fortunes);
        });
    }

    @RestMapping(name = "cached-worlds", auth = false)
    public CachedWorld[] cachedWorlds(int q) {
        int size = Math.min(500, Math.max(1, q));
        IntStream ids = ThreadLocalRandom.current().ints(size, 1, 10001);
        return source.finds(CachedWorld.class, ids.boxed());
    }
}
