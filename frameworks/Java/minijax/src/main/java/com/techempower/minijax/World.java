package com.techempower.minijax;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@Entity
@Table(name = "world")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class World {

    @Id
    private int id;

    @Column(name = "randomNumber", nullable = false)
    private int randomNumber;

    public World() {
    }

    public World(final int id, final int randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    public int getId() {
        return id;
    }

    public int getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(final int randomNumber) {
        this.randomNumber = randomNumber;
    }
}