package com.techempower.act.pgsql.domain;


import act.db.DB;
import org.beetl.sql.core.mapper.BaseMapper;

import javax.persistence.Entity;

@DB("pgsql")
@Entity
public class World extends com.techempower.act.sql.domain.World {
    public World(Integer id, Integer randomNumber) {
        super(id, randomNumber);
    }

    public static interface Dao extends BaseMapper<World> {}
}
