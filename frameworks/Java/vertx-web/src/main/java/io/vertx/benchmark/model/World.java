package io.vertx.benchmark.model;

import io.vertx.core.json.JsonObject;

import java.util.Collections;

/**
 * The model for the "world" database table.
 */
public final class World extends JsonObject {

  public static final String ID = "id";
  public static final String RANDOM_NUMBER = "randomNumber";

  /**
   * Constructs a new world object with the given parameters.
   *
   * @param id the ID of the world
   * @param randomNumber the random number of the world
   */
  public World(int id, int randomNumber) {
    put(ID, id);
    put(RANDOM_NUMBER, randomNumber);
  }

  public World(JsonObject doc) {
    super(doc == null ? Collections.emptyMap() : doc.getMap());
  }

  public int getId() {
    return getInteger(ID);
  }

  public int getRandomNumber() {
    return getInteger(RANDOM_NUMBER);
  }
}