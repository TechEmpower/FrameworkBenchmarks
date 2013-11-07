package com.techempower.spring.domain;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

@Entity
public final class Fortune implements Comparable<Fortune> {

    @Id
    @GeneratedValue
    private volatile Integer id;

    public volatile String message;

    Fortune() {
    }

    public Fortune(Integer id, String message) {
        this.id = id;
        this.message = message;
    }

    public Integer getId() {
        return this.id;
    }

    public String getMessage() {
        return this.message;
    }

    /**
     * For our purposes, Fortunes sort by their message text.
     */
    @Override
    public int compareTo(Fortune other) {
        return message.compareTo(other.message);
    }
}
