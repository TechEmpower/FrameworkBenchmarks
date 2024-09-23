/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.*;
import java.util.concurrent.*;
import java.util.stream.Stream;
import org.redkale.annotation.*;
import org.redkale.net.http.*;
import org.redkale.service.AbstractService;
import org.redkale.source.DataSource;
import org.redkale.util.AnyValue;

/**
 * 测试redkale-jdbc, 需要覆盖到原BenchmarkService
 *
 * @author zhangjx
 */
@RestService(name = " ", repair = false)
public class BenchmarkService extends AbstractService {

    private static final byte[] helloBytes = "Hello, world!".getBytes();

    @Resource
    private DataSource source;

    public void init(AnyValue conf) {
        source.finds(CachedWorld.class, 1);
    }
    
    @NonBlocking
    @RestMapping(auth = false)
    public byte[] plaintext() {
        return helloBytes;
    }

    @NonBlocking
    @RestMapping(auth = false)
    public Message json() {
        return new Message("Hello, World!");
    }

    @RestMapping(auth = false)
    public World db() {
        return source.find(World.class, ThreadLocalRandom.current().nextInt(10000) + 1);
    }

    @RestMapping(auth = false)
    public List<World> queries(int q) {
        return source.findsList(World.class, random(q));
    }

    @RestMapping(auth = false)
    public List<World> updates(int q) {
        int size = Math.min(500, Math.max(1, q));
        int[] newNumbers = ThreadLocalRandom.current().ints(size, 1, 10001).toArray();
        List<World> words = source.findsList(World.class, random(q));
        source.update(World.updateNewNumbers(words, newNumbers));
        return words;
    }

    @RestMapping(auth = false)
    public HttpScope fortunes() {
        List<Fortune> fortunes = source.queryList(Fortune.class);
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        return HttpScope.refer("").referObj(fortunes);
    }

    @NonBlocking
    @RestMapping(name = "cached-worlds", auth = false)
    public CachedWorld[] cachedWorlds(int q) {
        return source.finds(CachedWorld.class, random(q));
    }

    private Stream<Integer> random(int q) {
        int size = Math.min(500, Math.max(1, q));
        return ThreadLocalRandom.current().ints(size, 1, 10001).boxed();
    }
}
