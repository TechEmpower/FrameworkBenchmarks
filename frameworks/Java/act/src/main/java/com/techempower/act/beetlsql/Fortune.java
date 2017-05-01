package com.techempower.act.beetlsql;


import act.db.DB;
import org.beetl.sql.core.mapper.BaseMapper;

import javax.persistence.Entity;

@DB("beetl")
@Entity
public class Fortune extends com.techempower.act.sql.domain.Fortune {

    public Fortune(Integer id, String message) {
        super(id, message);
    }

    public interface Mapper extends BaseMapper<Fortune> {
    }

}
