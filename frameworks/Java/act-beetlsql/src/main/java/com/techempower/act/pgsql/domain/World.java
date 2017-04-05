package com.techempower.act.pgsql.domain;


import act.db.DB;
import act.util.SimpleBean;
import com.techempower.act.domain.IWorld;
import org.beetl.sql.core.annotatoin.AutoID;
import org.beetl.sql.core.mapper.BaseMapper;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

@DB("pgsql")
@Entity
public class World implements IWorld, SimpleBean {

    @Id
    @GeneratedValue
    private Integer id;

    @Column(name = "randomNumber")
    private Integer randomNumber;

    public World(Integer id, Integer randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    @AutoID
    public Integer getId() {
        return id;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public Integer getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(Integer randomNumber) {
        this.randomNumber = randomNumber;
    }


    public static interface Dao extends BaseMapper<World> {}
}
