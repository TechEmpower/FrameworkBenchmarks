package com.techempower.act.mongo.domain;

import act.db.DB;
import act.db.morphia.MorphiaDaoBase;
import act.util.SimpleBean;
import com.techempower.act.domain.IFortune;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Id;

@DB("mongo")
@Entity(noClassnameStored = true)
public final class Fortune implements IFortune, SimpleBean {

    @Id
    private Integer id;

    private String message;

    public Fortune(Integer id, String message) {
        this.id = id;
        this.message = message;
    }

    public Integer getId() {
        return id;
    }

    public String getMessage() {
        return this.message;
    }

    public static class Dao extends MorphiaDaoBase<Integer, Fortune> {}


}
