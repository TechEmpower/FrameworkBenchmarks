package com.techempower.beyondj.domain;

import com.google.gson.annotations.Expose;
import org.springframework.data.domain.Persistable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;

@Entity
public class Fortune implements Comparable<Fortune>, Persistable<Integer> {

    @Id
    @GeneratedValue
    @Expose
    private volatile Integer id;

    @Column
    @Expose
    public volatile String message;

    Fortune() {
    }

    public Fortune(Integer id, String message) {
        this.id = id;
        this.message = message;
    }

    public void setId(Integer id) {
        this.id = id;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public Integer getId() {
        return this.id;
    }

    @Override
    public boolean isNew() {
        return this.id == null;
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
