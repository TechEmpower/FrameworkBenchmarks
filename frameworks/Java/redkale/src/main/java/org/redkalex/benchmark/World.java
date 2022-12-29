/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import java.util.Arrays;
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

    public World randomNumber(int randomNumber) {
        this.randomNumber = randomNumber;
        return this;
    }

    public static World[] setNewNumbers(World[] worlds, int[] newNumbers) {
        for (int i = 0; i < worlds.length; i++) {
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
