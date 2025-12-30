package benchmark.model;

import io.micronaut.data.annotation.Id;
import io.micronaut.data.annotation.MappedEntity;
import io.micronaut.data.annotation.MappedProperty;

import java.util.Objects;

// Disable escape to have case-insensitive Postgres columns and table name and set lowercase name for MongoDB
@MappedEntity(value = "world", escape = false)
public class World {
    @Id
    private int id;
    @MappedProperty("randomNumber")
    private int randomNumber;

    public World() {
    }

    public World(int id, int randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getRandomNumber() {
        return randomNumber;
    }

    public void setRandomNumber(int randomNumber) {
        this.randomNumber = randomNumber;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        final World world = (World) o;
        return id == world.id && randomNumber == world.randomNumber;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, randomNumber);
    }
}
