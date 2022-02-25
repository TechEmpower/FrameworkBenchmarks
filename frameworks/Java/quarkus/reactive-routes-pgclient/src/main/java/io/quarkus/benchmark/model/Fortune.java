package io.quarkus.benchmark.model;

import java.util.Objects;

public class Fortune {

    private int id;
    private String message;

    public Fortune() {}

    public Fortune(int id, String message) {
        this.id = id;
        this.message = message;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o)
            return true;
        if (o == null || getClass() != o.getClass())
            return false;
        Fortune fortune = (Fortune) o;
        return id == fortune.id &&
                Objects.equals(message, fortune.message);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, message);
    }
}