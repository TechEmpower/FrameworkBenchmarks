package com.techempower.beyondj.domain;

import com.google.gson.annotations.Expose;
import org.springframework.data.domain.Persistable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

@Entity
public class World implements Persistable<Integer> {

    @Id
    @GeneratedValue
    @Expose
    private volatile Integer id;

    @Column
    @Expose
    private volatile Integer randomNumber;

    World() {
    }

    public World(Integer id, Integer randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }
    @Override
    public boolean isNew() {
        return this.id == null;
    }
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
}
