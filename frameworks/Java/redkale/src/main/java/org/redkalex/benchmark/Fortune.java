/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package org.redkalex.benchmark;

import org.redkale.convert.json.JsonConvert;
import org.redkale.persistence.*;

/**
 *
 * @author zhangjx
 */
@Entity
public final class Fortune implements Comparable<Fortune> {

    @Id
    private int id;

    private String message = "";

    public Fortune() {
    }

    public Fortune(int id, String message) {
        this.id = id;
        this.message = message;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public int compareTo(Fortune o) {
        return message.compareTo(o.message);
    }

    @Override
    public String toString() {
        return JsonConvert.root().convertTo(this);
    }

}
