package io.quarkus.benchmark.utils;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class LocalRandomTest {

    /**
     * Testing {@link LocalRandom#getNextRandom()}
     */
    @Test
    public void fullCoverage() {
        int[] results = new int[Randomizer.MAX_OF_RANGE + 1];
        final LocalRandom random = Randomizer.current();
        for (int i = 0; i < Randomizer.RANGE_SPACE; i++) {
            final int value = random.getNextRandom().intValue();
            results[value]++;
        }
        for (int i = 0; i < Randomizer.MIN_OF_RANGE; i++) {
            assertEquals(0, results[i]);
        }
        for (int i = Randomizer.MIN_OF_RANGE; i < Randomizer.MAX_OF_RANGE + 1; i++) {
            assertEquals(1, results[i]);
        }
        for (int i = 0; i < Randomizer.RANGE_SPACE; i++) {
            final int value = random.getNextRandom().intValue();
            results[value]++;
        }
        for (int i = Randomizer.MIN_OF_RANGE; i < Randomizer.MAX_OF_RANGE + 1; i++) {
            assertEquals(2, results[i]);
        }
    }

    /**
     * Testing {@link LocalRandom#getNextRandomExcluding(int)}
     */
    @Test
    public void fullCoverageExcept() {
        final int except = 37;
        int[] results = new int[Randomizer.MAX_OF_RANGE + 1];
        final LocalRandom random = Randomizer.current();
        for (int i = 0; i < Randomizer.RANGE_SPACE - 1; i++) {
            final int value = random.getNextRandomExcluding(except).intValue();
            results[value]++;
        }
        for (int i = 0; i < Randomizer.MIN_OF_RANGE; i++) {
            assertEquals(0, results[i]);
        }
        for (int i = Randomizer.MIN_OF_RANGE; i < Randomizer.MAX_OF_RANGE + 1; i++) {
            assertEquals((i == except ? 0 : 1), results[i]);
        }
        for (int i = 0; i < Randomizer.RANGE_SPACE - 1; i++) {
            final int value = random.getNextRandomExcluding(except).intValue();
            results[value]++;
        }
        for (int i = Randomizer.MIN_OF_RANGE; i < Randomizer.MAX_OF_RANGE + 1; i++) {
            assertEquals((i == except ? 0 : 2), results[i]);
        }
    }

}