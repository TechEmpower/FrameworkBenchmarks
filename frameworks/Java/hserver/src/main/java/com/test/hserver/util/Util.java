package com.test.hserver.util;

import java.util.concurrent.ThreadLocalRandom;

public class Util {
  public static int randomWorld() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }
}