package benchmark.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document
public final class Fortune implements Comparable<Fortune> {

    @Id
    public int id;

    public String message;

    public Fortune(int id, String message) {
        this.id = id;
        this.message = message;
    }

    @Override
    public int compareTo(final Fortune other) {
        return message.compareTo(other.message);
    }
}
