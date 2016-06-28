package app.helpers;

import java.util.concurrent.ThreadLocalRandom;

public final class Helper {
    
    public static final int NUMBER_OF_ROWS = 10_000;

    public static final int getRandomNumber() {
        return ThreadLocalRandom.current().nextInt(Helper.NUMBER_OF_ROWS) + 1;
    }

}
