/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.nio.ByteBuffer;
import java.util.*;
import java.util.concurrent.CompletableFuture;
import javax.annotation.Resource;
import org.redkale.net.http.*;
import org.redkale.service.AbstractService;
import org.redkale.source.DataSource;

/**
 *
 * @author zhangjx
 */
@RestService(name = " ", repair = false)
public class Service extends AbstractService {

    private static final ByteBuffer helloBuffer = ((ByteBuffer) ByteBuffer.allocateDirect("Hello, world!".length()).put("Hello, world!".getBytes()).flip()).asReadOnlyBuffer();

    private final Random random = new Random();

    @Resource
    private DataSource source;

    @RestMapping(name = "json")
    public Message getHelloMessage() {
        return new Message("Hello, World!");
    }

    @RestMapping(name = "plaintext")
    public ByteBuffer getHelloBuffer() {
        return helloBuffer.duplicate();
    }

    @RestMapping(name = "db")
    public CompletableFuture<World> findWorld() {
        return source.findAsync(World.class, randomId());
    }

    @RestMapping(name = "queries")
    public CompletableFuture<World[]> queryWorld(@RestParam(name = "queries") int count) {
        count = Math.min(500, Math.max(1, count));
        final World[] rs = new World[count];
        final CompletableFuture<World>[] futures = new CompletableFuture[count];
        for (int i = 0; i < count; i++) {
            final int index = i;
            futures[index] = source.findAsync(World.class, randomId()).whenComplete((w, t) -> rs[index] = w);
        }
        return CompletableFuture.allOf(futures).thenApply((r) -> rs);
    }

    @RestMapping(name = "updates")
    public CompletableFuture<World[]> updateWorld(@RestParam(name = "queries") int count) {
        count = Math.min(500, Math.max(1, count));
        final World[] rs = new World[count];
        final CompletableFuture<World>[] futures = new CompletableFuture[count];
        for (int i = 0; i < count; i++) {
            final int index = i;
            futures[index] = source.findAsync(World.class, randomId()).whenComplete((w, t) -> {
                rs[index] = w;
                rs[index].setRandomNumber(randomId());
            });
        }
        return CompletableFuture.allOf(futures).thenCompose((r) -> { 
            return source.updateAsync(rs).thenApply((v) -> rs);
        });
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
