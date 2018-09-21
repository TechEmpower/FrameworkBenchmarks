package benchmark.entity;

import java.util.Objects;

/**
 * The model for the "world" database table.
 */
public final class World {

    private int id;
    private int randomNumber;

    public World() {
        super();
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
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        World world = (World) o;
        return id == world.id &&
                randomNumber == world.randomNumber;
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, randomNumber);
    }
}