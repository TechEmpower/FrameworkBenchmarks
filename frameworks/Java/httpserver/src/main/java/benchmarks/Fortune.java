package benchmarks;

public class Fortune implements Comparable<Fortune> {

    private final int id;
    private final String message;

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
