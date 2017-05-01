package com.techempower.act.beetlsql;


import act.db.DB;
import org.beetl.sql.core.mapper.BaseMapper;

import javax.persistence.Entity;

@DB("beetl")
@Entity
public class World extends com.techempower.act.sql.domain.World {
    public World(Integer id, Integer randomNumber) {
        super(id, randomNumber);
    }

    public interface Mapper extends BaseMapper<World> {}
}
