package com.litongjava.tio.http.server.utils;

import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

public class RandomUtils {

  private static final int MIN_WORLD_NUMBER = 1;
  private static final int MAX_WORLD_NUMBER_PLUS_ONE = 10_001;

  public static int randomWorldNumber() {
    return ThreadLocalRandom.current().nextInt(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER_PLUS_ONE);
  }

  public static IntStream randomWorldNumbers() {
    return ThreadLocalRandom.current().ints(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER_PLUS_ONE)
        // distinct() allows us to avoid using Hibernate's first-level cache in
        // the JPA-based implementation. Using a cache like that would bypass
        // querying the database, which would violate the test requirements.
        .distinct();
  }

  public static int parseQueryCount(String textValue) {
    if (textValue == null) {
      return 1;
    }
    int parsedValue;
    try {
      parsedValue = Integer.parseInt(textValue);
    } catch (NumberFormatException e) {
      return 1;
    }
    return Math.min(500, Math.max(1, parsedValue));
  }
}
