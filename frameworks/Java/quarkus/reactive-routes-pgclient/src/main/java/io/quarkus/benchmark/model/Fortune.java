package io.quarkus.benchmark.model;

public class Fortune implements Comparable<Fortune> {

    private final int id;
    private final String message;

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
    public int compareTo(final Fortune other) {
        return message.compareTo(other.message);
    }
}