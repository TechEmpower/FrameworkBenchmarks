package benchmark;

import java.util.concurrent.ThreadLocalRandom;

public abstract class BaseDBController {
    protected int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    protected int parseQueryCount(String textValue) {
        if (textValue == null) {
            return 1;
        }
        int parsedValue;
        try {
            parsedValue = Integer.parseInt(textValue);
        } catch (NumberFormatException e) {
            return 1;
        }
        return Math.min(500, Math.max(1, parsedValue));
    }
}
