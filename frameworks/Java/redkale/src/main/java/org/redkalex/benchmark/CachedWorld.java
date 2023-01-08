package org.redkalex.benchmark;

import java.util.*;
import java.util.function.IntFunction;
import java.util.stream.IntStream;
import org.redkale.convert.json.JsonConvert;
import org.redkale.persistence.*;
import org.redkale.source.DataSource;

/**
 *
 * @author zhangjx
 */
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

    public static class Cache {

        private static Cache instance;

        static Cache getInstance(DataSource source) {
            if (instance == null) {
                synchronized (Cache.class) {
                    if (instance == null) {
                        instance = new Cache(source);
                    }
                }
            }
            return instance;
        }

        private CachedWorld[] array;

        private IntFunction<CachedWorld> mapFunc = c -> array[c];

        private IntFunction<CachedWorld[]> arrayFunc = c -> new CachedWorld[c];

        public Cache(DataSource source) {
            List<CachedWorld> list = source.queryList(CachedWorld.class);
            this.array = list.toArray(new CachedWorld[list.size()]);
        }

        public CachedWorld[] random(Random random, int size) {
            IntStream ids = random.ints(size, 0, 10000);
            return ids.mapToObj(mapFunc).toArray(arrayFunc);
        }
    }
}
