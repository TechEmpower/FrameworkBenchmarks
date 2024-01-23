package io.quarkus.benchmark.utils;

import io.netty.util.concurrent.FastThreadLocal;

import java.util.ArrayList;

/**
 * The rules of the benchmark frequently require randomly generated numbers
 * in the range from 1 to 10000.
 * Often multiple numbers are needed, and in this case we need to avoid duplicates
 * because otherwise the ORM optimisations will invalidate our operations
 * (Hibernate ORM will skip unnecessary operations but this is specifically disallowed,
 * and it's not possible to disable this behaviour in ORM as it's an intrinsic
 * aspect of correctness of an ORM).
 * Because of this twist in the rules, we're better off writing a custom helper
 * than making vanilla use of the Java platform randomizer.
 */
public final class Randomizer {

    static final short MIN_OF_RANGE = 1;
    static final short MAX_OF_RANGE = 10000;
    static final short RANGE_SPACE = MAX_OF_RANGE - MIN_OF_RANGE + 1;
    private final static Integer[] randomSequenceBoxed = initRange();

    private static final FastThreadLocal<ThreadlocalizedRandomizer> localRandom = new FastThreadLocal<>() {
        @Override
        protected io.quarkus.benchmark.utils.Randomizer.ThreadlocalizedRandomizer initialValue() {
            return new ThreadlocalizedRandomizer();
        }
    };

    public static LocalRandom current() {
        return localRandom.get();
    }

    private static Integer[] initRange() {
        ArrayList<Integer> boxedSequence = new java.util.ArrayList<>(MAX_OF_RANGE);
        short value = MIN_OF_RANGE;
        for (int i = 0; i < MAX_OF_RANGE; i++) {
            boxedSequence.add(Integer.valueOf(value++));
        }
        java.util.Collections.shuffle(boxedSequence);
        return boxedSequence.toArray(new Integer[0]);
    }

    private static final class ThreadlocalizedRandomizer implements LocalRandom {

        private final java.util.concurrent.ThreadLocalRandom random;
        private int currentIndex;

        private ThreadlocalizedRandomizer() {
            this.random = java.util.concurrent.ThreadLocalRandom.current();
            this.currentIndex = random.nextInt(RANGE_SPACE);
        }

        @Override
        public Integer getNextRandom() {
            currentIndex++;
            if (currentIndex == MAX_OF_RANGE) {
                currentIndex = 0;
            }
            return randomSequenceBoxed[currentIndex];
        }

        @Override
        public Integer getNextRandomExcluding(int exclusion) {
            final Integer nextRandom = getNextRandom();
            if (nextRandom.intValue() == exclusion) {
                //Since it's a sequence of shuffled unique numbers, and this is consumed by a single thread,
                //we know at this stage the next try will be different for sure.
                return getNextRandom();
            }
            return nextRandom;
        }

    }

}
