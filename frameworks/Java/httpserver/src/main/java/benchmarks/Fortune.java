package benchmarks;

public record Fortune(int id, String message) implements Comparable<Fortune> {

    @Override
    public int compareTo(Fortune other) {
        return message.compareTo(other.message);
    }
}
