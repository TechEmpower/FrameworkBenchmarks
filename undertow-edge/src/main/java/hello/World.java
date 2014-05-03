package hello;

/**
 * The model for the "world" database table.
 */
public final class World {
    public int id;
    public int randomNumber;

    /**
     * Constructs a new world object with the given parameters.
     *
     * @param id           the ID of the world
     * @param randomNumber the random number of the world
     */
    public World(int id, int randomNumber) {
        this.id = id;
        this.randomNumber = randomNumber;
    }
}
