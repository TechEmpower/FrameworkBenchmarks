/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.*;
import org.redkale.convert.json.JsonConvert;
import org.redkale.persistence.*;

/**
 *
 * @author zhangjx
 */
@Entity
public final class World implements Comparable<World> {

    @Id
    private int id;

    private int randomNumber;

    public static World[] updateNewNumbers(List<World> list, int[] newNumbers) {
        World[] worlds = new World[list.size()];
        for (int i = 0; i < worlds.length; i++) {
            worlds[i] = list.get(i);
            worlds[i].randomNumber = newNumbers[i];
        }
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
