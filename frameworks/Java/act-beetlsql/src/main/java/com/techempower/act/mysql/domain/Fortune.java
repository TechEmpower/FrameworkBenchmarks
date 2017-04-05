package com.techempower.act.mysql.domain;


import act.db.DB;
import org.beetl.sql.core.mapper.BaseMapper;

import javax.persistence.Entity;

@DB("mysql")
@Entity
public class Fortune extends com.techempower.act.sql.domain.Fortune {

    public Fortune(Integer id, String message) {
        super(id, message);
    }

    public interface Dao extends BaseMapper<Fortune> {}
}
