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
    private int id;

    @JsonProperty
    @Column(name = "randomNumber", nullable = false)
    private int randomNumber;

    public World() {
    }

    public int getId() {
        return id;
    }

    public int getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(int randomNumber) {
        this.randomNumber = randomNumber;
    }
}
