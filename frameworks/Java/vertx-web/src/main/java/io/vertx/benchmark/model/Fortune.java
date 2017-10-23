package io.vertx.benchmark.model;

import io.vertx.core.json.JsonObject;

/**
 * The model for the "fortune" database table.
 */
public final class Fortune implements Comparable<Fortune> {

  private final int id;
  private final String message;

  /**
   * Constructs a new fortune object with the given parameters.
   *
   * @param id the ID of the fortune
   * @param message the message of the fortune
   */
  public Fortune(int id, String message) {
    this.id = id;
    this.message = message;
  }

  public Fortune(JsonObject doc) {
    this.id = doc.getInteger("id");
    this.message = doc.getString("message");
  }

  public int getId() {
    return id;
  }

  public String getMessage() {
    return message;
  }

  @Override
  public int compareTo(Fortune other) {
    return message.compareTo(other.message);
  }
}