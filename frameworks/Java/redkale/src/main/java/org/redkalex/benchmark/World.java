/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.*;
import javax.persistence.*;
import org.redkale.convert.json.JsonConvert;

/**
 *
 * @author zhangjx
 */
@Entity
public final class World implements Comparable<World> {

    @Id
    private int id;

    private int randomNumber;

    public World randomNumber(int randomNumber) {
        this.randomNumber = randomNumber;
        return this;
    }

    public static World[] sort(World[] worlds) {
        Arrays.sort(worlds);
        return worlds;
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
    public int compareTo(World o) {
        return Integer.compare(id, o.id);
    }

    @Override
    public String toString() {
        return JsonConvert.root().convertTo(this);
    }

}
