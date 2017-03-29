package com.techempower.act.mysql.domain;


import act.db.DB;
import act.db.ebean.EbeanDao;

import javax.persistence.Entity;

@DB("mysql")
@Entity
public class Fortune extends com.techempower.act.sql.domain.Fortune {

    public Fortune(Integer id, String message) {
        super(id, message);
    }

    public static class Dao extends EbeanDao<Integer, Fortune> {}
}
