package com.techempower;

import io.jooby.Context;
import io.jooby.value.Value;

import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

public class Util {
  private static final Integer[] BOXED_RND = IntStream.range(1, 10001).boxed().toArray(Integer[]::new);

  public static int queries(Context ctx) {
    try {
      Value queries = ctx.query("queries");
      return queries.isMissing()
          ? 1
          : Math.min(500, Math.max(1, Integer.parseInt(queries.value())));
    } catch (NumberFormatException x) {
      return 1;
    }
  }

  public static int randomWorld() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }

  public static int boxedRandomWorld() {
    final int rndValue = ThreadLocalRandom.current().nextInt(1, 10001);
    return BOXED_RND[rndValue - 1];
  }
}
