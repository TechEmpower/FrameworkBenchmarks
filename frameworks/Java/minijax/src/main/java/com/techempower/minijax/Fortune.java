package com.techempower.minijax;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.NamedQuery;
import javax.persistence.Table;
import javax.xml.bind.annotation.XmlAccessType;
import javax.xml.bind.annotation.XmlAccessorType;
import javax.xml.bind.annotation.XmlRootElement;

@Entity
@Table(name = "fortune")
@NamedQuery(name = "Fortune.getAll", query = "SELECT f FROM Fortune f")
@XmlRootElement
@XmlAccessorType(XmlAccessType.FIELD)
public class Fortune implements Comparable<Fortune> {

    @Id
    private int id;

    @Column(name = "message", nullable = false)
    private String message;

    public Fortune() {
    }

    public Fortune(final String message) {
        this.message = message;
    }

    public Fortune(final int id, final String message) {
        this.id = id;
        this.message = message;
    }

    public int getId() {
        return id;
    }

    public String getMessage() {
        return message;
    }

    @Override
    public int compareTo(final Fortune o) {
        return message.compareTo(o.message);
    }
}
