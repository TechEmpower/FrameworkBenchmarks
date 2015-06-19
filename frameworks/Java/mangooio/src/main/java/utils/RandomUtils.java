package utils;

import interfaces.Constants;

import java.util.concurrent.ThreadLocalRandom;

public final class RandomUtils {
	public static int getRandomId() {
		return ThreadLocalRandom.current().nextInt(Constants.ROWS) + 1;
	}
}
