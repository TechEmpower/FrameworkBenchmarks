package vertx.model;

import io.vertx.core.json.JsonObject;

import java.util.Map;

/**
 * The model for the "world" database table.
 */
public final class CachedWorld extends JsonObject implements Comparable<CachedWorld> {

  private final int id;
  private final int randomNumber;

  /**
   * Constructs a new world object with the given parameters.
   *
   * @param id the ID of the world
   * @param randomNumber the random number of the world
   */
  public CachedWorld(int id, int randomNumber) {
    super(Map.of("id", id, "randomNumber", randomNumber));
    this.id = id;
    this.randomNumber = randomNumber;
  }

  public int getId() {
    return id;
  }

  public int getRandomNumber() {
    return randomNumber;
  }

  @Override
  public int compareTo(CachedWorld o) {
    return Integer.compare(id, o.id);
  }
}