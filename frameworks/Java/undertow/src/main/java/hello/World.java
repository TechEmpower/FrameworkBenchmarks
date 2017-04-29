package hello;

/**
 * The model for the "world" database table.
 */
public final class World {
  public int id;
  public int randomNumber;

  public World(int id, int randomNumber) {
    this.id = id;
    this.randomNumber = randomNumber;
  }
}
