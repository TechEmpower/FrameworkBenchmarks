/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

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

        private Object[] array;

        public WorldEntityCache(DataSource source) {
            this.array = source.queryList(CachedWorld.class).toArray();
        }

        public CachedWorld findAt(int index) {
            return (CachedWorld) array[index];
        }
    }
}
