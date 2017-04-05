package com.techempower.act.mysql.domain;


import act.db.DB;
import org.beetl.sql.core.mapper.BaseMapper;

import javax.persistence.Entity;

@DB("mysql")
@Entity
public class World extends com.techempower.act.sql.domain.World {

    public World(Integer id, Integer randomNumber) {
        super(id, randomNumber);
    }

    public static interface Dao extends BaseMapper<World> {}
}
