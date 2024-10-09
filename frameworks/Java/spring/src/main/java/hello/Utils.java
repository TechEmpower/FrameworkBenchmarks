package hello;

import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.IntStream;

abstract public class Utils {

	private static final int MIN_WORLD_NUMBER = 1;
	private static final int MAX_WORLD_NUMBER_PLUS_ONE = 10_001;

	public static int randomWorldNumber() {
		return ThreadLocalRandom.current().nextInt(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER_PLUS_ONE);
	}

	public static IntStream randomWorldNumbers() {
		return ThreadLocalRandom.current().ints(MIN_WORLD_NUMBER, MAX_WORLD_NUMBER_PLUS_ONE).distinct();
	}

}
