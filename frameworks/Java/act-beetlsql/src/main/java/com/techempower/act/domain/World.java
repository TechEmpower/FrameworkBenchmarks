package com.techempower.act.domain;


import act.util.SimpleBean;
import org.beetl.sql.core.annotatoin.AutoID;
import org.beetl.sql.core.mapper.BaseMapper;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

@Entity
public class World implements SimpleBean {


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

    public interface Dao extends BaseMapper<World> {
    }
}
