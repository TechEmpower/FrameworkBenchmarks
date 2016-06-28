package com.example.helloworld.db.model;

import com.fasterxml.jackson.annotation.JsonProperty;

import javax.persistence.*;

@Entity
@Table(name = "Fortune")
public class Fortune implements Comparable<Fortune> {

    @Id
    @JsonProperty
    private int id;

    @JsonProperty
    @Column(name = "message", nullable = false)
    private String message;

    @SuppressWarnings("unused")
    public Fortune() {}

    public Fortune(String message) {
        this.message = message;
    }

    public int getId() {
        return id;
    }

    public String getMessage() {
        return message;
    }

    @Override
    public int compareTo(Fortune o) {
        return message.compareTo(o.message);
    }
}
