package com.techempower.ee7.util;

import java.util.concurrent.ThreadLocalRandom;

public class Helpers {

  /**
   * Random number between 1 and 10,000
   * 
   * @return
   */
  public static int randomWorldId() {
    return ThreadLocalRandom.current().nextInt(1, 10001);
  }

  /**
   * Returns a bounded integer. min if null or less than bounds. max if greater than bounds
   * 
   * @param value
   * @param min
   * @param max
   * @return
   */
  public static int boundedIntegerFromNullableString(final String value, final int min,
      final int max) {
    if (value == null) {
      return min;
    } else {
      try {
        return Math.min(max, Math.max(min, Integer.parseInt(value)));
      } catch (NumberFormatException e) {
        return min;
      }
    }
  }
}
