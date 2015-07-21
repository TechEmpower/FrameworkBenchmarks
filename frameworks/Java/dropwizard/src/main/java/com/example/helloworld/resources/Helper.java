package com.example.helloworld.resources;

import com.google.common.base.Optional;

import java.util.concurrent.ThreadLocalRandom;

/**
 * Provides utility methods for the benchmark tests.
 * Taken from undertow-edge project.
 */
public final class Helper {
    private Helper() {
        throw new AssertionError();
    }

    static int getQueries(Optional<String> queries) {
        if (!queries.isPresent()) {
            return 1;
        }
        try {
            int parsedValue = Integer.parseInt(queries.get());
            return Math.min(500, Math.max(1, parsedValue));
        } catch (NumberFormatException e) {
            return 1;
        }
    }

    public static int randomWorld() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }
}