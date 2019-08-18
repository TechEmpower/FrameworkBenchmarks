package com.techempower;

import io.jooby.Context;
import io.jooby.Value;

import java.util.concurrent.ThreadLocalRandom;

public class Util {

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
}
