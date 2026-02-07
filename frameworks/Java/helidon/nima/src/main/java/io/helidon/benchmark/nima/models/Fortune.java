
package io.helidon.benchmark.nima.models;

public final class Fortune implements Comparable<Fortune> {
    public int id;
    public String message;

    public Fortune(int id, String message) {
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
    public int compareTo(Fortune other) {
        return message.compareTo(other.message);
    }
}
