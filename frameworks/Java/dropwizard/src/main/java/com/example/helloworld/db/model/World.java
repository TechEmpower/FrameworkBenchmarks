package com.example.helloworld.db.model;

import javax.persistence.*;

@Entity
@Table(name = "World")
public class World {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private long id;

    @Column(name = "randomNumber", nullable = false)
    private long randomNumber;

    public World() {}

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
