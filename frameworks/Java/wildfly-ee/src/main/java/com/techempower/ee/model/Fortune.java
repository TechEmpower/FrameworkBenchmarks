package com.techempower.ee7.model;

import java.io.Serializable;

import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.NamedQuery;
import jakarta.validation.constraints.NotNull;
import jakarta.validation.constraints.Size;

@NamedQuery(name = "allFortunes", query = "SELECT f FROM Fortune f")
@Entity
public class Fortune implements Comparable<Fortune>, Serializable {

    private static final long serialVersionUID = 1L;

    private int id;
    private String message;

    public Fortune() {

    }

    public Fortune(int id, String message) {
        this.id = id;
        this.message = message;
    }

    @Id
    @GeneratedValue(strategy = GenerationType.AUTO)
    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    @NotNull
    @Size(max = 2048)
    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public int compareTo(Fortune o) {
        return message.compareTo(o.getMessage());
    }
}
