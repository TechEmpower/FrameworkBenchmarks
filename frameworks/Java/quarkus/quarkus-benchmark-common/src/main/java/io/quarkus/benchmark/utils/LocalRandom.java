package io.quarkus.benchmark.utils;

public interface LocalRandom {

    /**
     * @return an Integer representing a random number in the space expected by the benchmark: [1-10000].
     */
    Integer getNextRandom();

    /**
     * Also according to benchmark requirements, except that in this special case
     * of the update test we need to ensure we'll actually generate an update operation:
     * for this we need to generate a random number between 1 to 10000, but different
     * from the current field value.
     *
     * @param exclusion
     * @return an Integer representing a random number in the space expected by the benchmark: [1-10000],
     * but always excluding the one matching exclusion.
     */
    Integer getNextRandomExcluding(int exclusion);
}
