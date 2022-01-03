package io.vertx.benchmark.model;

import io.vertx.core.json.JsonObject;

/**
 * The model for the "world" database table.
 */
public final class World implements Comparable<World> {

  private final int id;
  private final int randomNumber;

  /**
   * Constructs a new world object with the given parameters.
   *
   * @param id the ID of the world
   * @param randomNumber the random number of the world
   */
  public World(int id, int randomNumber) {
    this.id = id;
    this.randomNumber = randomNumber;
  }

  public World(JsonObject doc) {
    this.id = doc.getInteger("id");
    this.randomNumber = doc.getInteger("randomNumber");
  }

  public int getId() {
    return id;
  }

  public int getRandomNumber() {
    return randomNumber;
  }

  @Override
  public int compareTo(World o) {
    return Integer.compare(getId(), o.getId());
  }
}