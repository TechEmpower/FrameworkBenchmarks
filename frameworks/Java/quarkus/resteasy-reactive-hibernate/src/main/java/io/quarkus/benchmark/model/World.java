package io.quarkus.benchmark.model;

import jakarta.persistence.Entity;
import jakarta.persistence.Id;

@Entity
public class World {

    @Id
    private int id;
    private int randomNumber;

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(int randomNumber) {
        this.randomNumber = randomNumber;
    }

}