package io.quarkus.benchmark.resource;

import io.vertx.core.http.HttpServerRequest;

public class HttpQueryParameterUtils {

    public static int queriesFrom(final HttpServerRequest request) {
        final String param = request.getParam("queries");

        if (param == null) {
            return 1;
        }
        try {
            final int parsedValue = Integer.parseInt(param);
            return Math.min(500, Math.max(1, parsedValue));
        } catch (final NumberFormatException e) {
            return 1;
        }
    }
}
