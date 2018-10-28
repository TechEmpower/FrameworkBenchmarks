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

    @RestMapping(name = "db")
    public CompletableFuture<World> findWorld() {
        return source.findAsync(World.class, randomId());
    }

    @RestMapping(name = "queries")
    public World[] queryWorld(@RestParam(name = "queries") int count) {
        count = Math.min(500, Math.max(1, count));
        final World[] rs = new World[count];
        for (int i = 0; i < count; i++) {
            rs[i] = source.find(World.class, randomId());
        }
        return rs;
    }

    @RestMapping(name = "updates")
    public World[] updateWorld(@RestParam(name = "queries") int count) {
        count = Math.min(500, Math.max(1, count));
        final World[] rs = new World[count];
        for (int i = 0; i < count; i++) {
            rs[i] = source.find(World.class, randomId());
            rs[i].setRandomNumber(randomId());
        }
		source.update(rs);
        return rs;
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
