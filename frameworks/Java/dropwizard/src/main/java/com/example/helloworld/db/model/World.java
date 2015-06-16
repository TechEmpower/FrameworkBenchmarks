package com.example.helloworld.db.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "World")
public class World {

    @Id
    @JsonProperty
    private long id;

    @JsonProperty
    @Column(name = "randomNumber", nullable = false)
    private long randomNumber;

    public World() {
    }

    public long getId() {
        return id;
    }

    public long getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(long randomNumber) {
        this.randomNumber = randomNumber;
    }
}
