package io.tadx.benchmark.entity;

import io.tadx.core.data.EntityBase;
import io.tadx.core.data.annotation.Column;
import io.tadx.core.data.annotation.Entity;
import io.tadx.core.data.annotation.OutputField;
import io.tadx.core.data.annotation.PrimaryKey;

@Entity(tableName = "fortune")
public class Fortune extends EntityBase implements Comparable<Fortune> {

    public Fortune() {
    }

    public Fortune(int id, String message) {
        this.id = id;
        this.message = message;
    }

    @PrimaryKey
    @OutputField
    public Integer id;

    @Column
    @OutputField
    public String message;

    @Override
    public int compareTo(Fortune o) {
        return message.compareTo(o.message);
    }
}
