/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.*;
import java.util.concurrent.*;
import javax.persistence.*;
import org.redkale.convert.json.JsonConvert;
import org.redkale.source.*;

/**
 *
 * @author zhangjx
 */
//@Cacheable(direct = true)
@Entity
@Table(name = "World")
public final class CachedWorld implements Comparable<CachedWorld> {

    @Id
    private int id;

    private int randomNumber;

    public CachedWorld randomNumber(int randomNumber) {
        this.randomNumber = randomNumber;
        return this;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(int randomNumber) {
        this.randomNumber = randomNumber;
    }

    @Override
    public int compareTo(CachedWorld o) {
        return Integer.compare(id, o.id);
    }

    @Override
    public String toString() {
        return JsonConvert.root().convertTo(this);
    }

    public static class WorldEntityCache {

        private CachedWorld[] array;

        public WorldEntityCache(DataSource source) {
            List<CachedWorld> list = CompletableFuture.supplyAsync(
                () -> source.queryList(CachedWorld.class),
                ForkJoinPool.commonPool()).join();
            this.array = list.toArray(new CachedWorld[list.size()]);
        }

        public CachedWorld findAt(int index) {
            return (CachedWorld) array[index];
        }

        public CachedWorld[] random(Random random, int size) {
            Random rand = random;
            final CachedWorld[] worlds = new CachedWorld[size];
            for (int i = 0; i < worlds.length; i++) {
                long index = Math.abs(rand.nextLong()) % 10000;
                worlds[i] = array[(int) index];
            }
            return worlds;
        }
    }
}
