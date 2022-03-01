package com.test.hserver.util;

import java.util.concurrent.ThreadLocalRandom;

public class Util {
  public static int randomWorld() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }

  public static int getQueries(String queries) {
    try {
      int count = Integer.parseInt(queries);
      return Math.min(500, Math.max(1, count));
    } catch (Exception e) {
      return 1;
    }
  }
}