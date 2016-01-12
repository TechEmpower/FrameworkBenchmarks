package io.vertx.benchmark.model;

import io.vertx.core.json.JsonObject;

import java.util.Collections;

/**
 * The model for the "fortune" database table.
 */
public final class Fortune extends JsonObject implements Comparable<Fortune> {

  private static final String ID = "id";
  private static final String MESSAGE = "message";

  /**
   * Constructs a new fortune object with the given parameters.
   *
   * @param id the ID of the fortune
   * @param message the message of the fortune
   */
  public Fortune(int id, String message) {
    put(ID, id);
    put(MESSAGE, message);
  }

  public Fortune(JsonObject doc) {
    super(doc == null ? Collections.emptyMap() : doc.getMap());
  }

  public int getId() {
    return getInteger(ID);
  }

  public String getMessage() {
    return getString(MESSAGE);
  }

  @Override
  public int compareTo(Fortune other) {
    return getMessage().compareTo(other.getMessage());
  }
}