package com.example.helloworld.db.model;

import javax.persistence.*;

@Entity
@Table(name = "Fortune")
public class Fortune implements Comparable<Fortune> {

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    private long id;

    @Column(name = "message", nullable = false)
    private String message;

    @SuppressWarnings("unused")
    public Fortune() {}

    public Fortune(String message) {
        this.message = message;
    }

    public long getId() {
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
