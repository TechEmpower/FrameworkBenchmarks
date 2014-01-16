package com.techempower.ee7.util;

import java.util.Date;
import java.util.concurrent.ThreadLocalRandom;

import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.ResponseBuilder;

public class Helpers {

  private static final String JSON_CONTENT_TYPE = "application/json; charset=UTF-8";
  private static final String PLAIN_TEXT_CONTENT_TYPE = "text/plain; charset=UTF-8";
  private static final String HEADER_SERVER_NAME = "Server";
  private static final String HEADER_SERVER_VALUE = "Wildfly, U-tow";
  private static final String HEADER_DATE_NAME = "Date";

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

  public static Response planTextResponse(Object entity) {
    return baseResponse(entity).type(PLAIN_TEXT_CONTENT_TYPE).build();
  }

  public static Response jsonResponse(Object entity) {
    return baseResponse(entity).type(JSON_CONTENT_TYPE).build();
  }

  private static ResponseBuilder baseResponse(Object entity) {
    return Response.ok(entity).header(HEADER_SERVER_NAME, HEADER_SERVER_VALUE)
        .header(HEADER_DATE_NAME, new Date());
  }
}
