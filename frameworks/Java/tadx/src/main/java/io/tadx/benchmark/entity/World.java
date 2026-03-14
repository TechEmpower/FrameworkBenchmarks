package io.tadx.benchmark.entity;

import io.tadx.core.data.EntityBase;
import io.tadx.core.data.annotation.Column;
import io.tadx.core.data.annotation.Entity;
import io.tadx.core.data.annotation.OutputField;
import io.tadx.core.data.annotation.PrimaryKey;

@Entity(tableName = "world")
public class World extends EntityBase {

    @PrimaryKey
    @OutputField
    public Integer id;

    @Column
    @OutputField
    public Integer randomnumber;
}
