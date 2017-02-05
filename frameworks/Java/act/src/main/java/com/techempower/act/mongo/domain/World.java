package com.techempower.act.mongo.domain;


import act.db.DB;
import act.db.morphia.MorphiaDaoBase;
import com.techempower.act.domain.IWorld;
import org.mongodb.morphia.annotations.Entity;
import org.mongodb.morphia.annotations.Id;

@DB("mongo")
@Entity(value = "world", noClassnameStored = true)
public final class World implements IWorld {

    @Id
    private Integer id;

    private Integer randomNumber;

    public World(Integer id, Integer randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    public Integer getId() {
        return id;
    }

    public Integer getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(Integer randomNumber) {
        this.randomNumber = randomNumber;
    }

    public static class Dao extends MorphiaDaoBase<Integer, World> {}
}
