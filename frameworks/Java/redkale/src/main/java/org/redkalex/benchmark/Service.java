/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.*;
import java.util.concurrent.CompletableFuture;
import javax.annotation.Resource;
import org.redkale.source.DataSource;

/**
 *
 * @author zhangjx
 */
@SuppressWarnings("unchecked")
public class Service extends org.redkale.service.AbstractService {

    private final Random random = new Random();

    @Resource
    private DataSource source;

    public World findWorld() {
        return source.find(World.class, randomId());
    }

    public CompletableFuture<World[]> queryWorld(int count) {
        count = Math.min(500, Math.max(1, count));
        final World[] rs = new World[count];
        final CompletableFuture<World>[] futures = new CompletableFuture[count];
        for (int i = 0; i < count; i++) {
            final int index = i;
            futures[i] = source.findAsync(World.class, randomId()).whenComplete((w, t) -> rs[index] = w);
        }
        return CompletableFuture.allOf(futures).thenApply((r) -> rs);
    }

    public CompletableFuture<World[]> updateWorld(int count) {
        count = Math.min(500, Math.max(1, count));
        final World[] rs = new World[count];
        final CompletableFuture<World>[] futures = new CompletableFuture[count];
        for (int i = 0; i < count; i++) {
            final int index = i;
            futures[i] = source.findAsync(World.class, randomId()).whenComplete((w, t) -> {
                rs[index] = w;
                rs[index].setRandomNumber(randomId());
            });
        }
        return CompletableFuture.allOf(futures).thenApply((r) -> {
            source.update(rs);
            return rs;
        });
    }

    public List<Fortune> queryFortune() {
        return source.queryList(Fortune.class);
    }

    private int randomId() {
        return 1 + random.nextInt(10000);
    }
}
