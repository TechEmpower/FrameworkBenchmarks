package com.example.helloworld.resources;

import java.util.concurrent.ThreadLocalRandom;

import com.google.common.base.Optional;

/**
 * Provides utility methods for the benchmark tests.
 * Taken from undertow-edge project.
 */
final class Helper {
  private Helper() {
    throw new AssertionError();
  }

  /**
   * Returns the value of the "queries" request parameter, which is an integer
   * bound between 1 and 500 with a default value of 1.
   *
   * @param exchange the current HTTP exchange
   * @return the value of the "queries" request parameter
   */
  static int getQueries(Optional<String> queries) {
    String value = queries.orNull();
    if (value == null) {
      return 1;
    }
    try {
      int parsedValue = Integer.parseInt(value);
      return Math.min(500, Math.max(1, parsedValue));
    } catch (NumberFormatException e) {
      return 1;
    }
  }

  /**
   * Returns a random integer that is a suitable value for both the {@code id}
   * and {@code randomNumber} properties of a world object.
   *
   * @return a random world number
   */
  static int randomWorld() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }
}