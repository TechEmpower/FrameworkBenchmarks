package com.example.helloworld.resources;

import java.util.Optional;
import java.util.concurrent.ThreadLocalRandom;

/**
 * Provides utility methods for the benchmark tests. Taken from undertow-edge
 * project.
 */
public final class Helper {
	private final static int MAX_RANDOM_NUMBER = 10000;

	private Helper() {
		throw new AssertionError();
	}

	static int getQueries(Optional<String> queries) {
		if (!queries.isPresent()) {
			return 1;
		}
		return getQueries(queries.get());
	}

	static int getQueries(String queries) {
		try {
			int parsedValue = Integer.parseInt(queries);
			return Math.min(500, Math.max(1, parsedValue));
		} catch (NumberFormatException e) {
			return 1;
		}
	}

	public static int randomWorld() {
		return 1 + ThreadLocalRandom.current().nextInt(MAX_RANDOM_NUMBER);
	}

	public static int[] getRandomInts(int count) {
		return ThreadLocalRandom.current().ints(1, MAX_RANDOM_NUMBER + 1).distinct().limit(count).sorted().toArray();
	}
}