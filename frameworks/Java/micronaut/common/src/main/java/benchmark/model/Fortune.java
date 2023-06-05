package benchmark.model;

import io.micronaut.data.annotation.Id;
import io.micronaut.data.annotation.MappedEntity;

import java.util.Objects;

// Disable escape to have case-insensitive Postgres columns and table name and set lowercase name for MongoDB
@MappedEntity(value = "fortune", escape = false)
public class Fortune {
    @Id
    private int id;
    private String message;

    public Fortune() {
    }

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
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final Fortune fortune = (Fortune) o;
        return id == fortune.id && Objects.equals(message, fortune.message);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, message);
    }
}
