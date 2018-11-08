package handlers;

import models.World;
import ratpack.handling.Context;
import ratpack.handling.InjectionHandler;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.concurrent.ThreadLocalRandom;

public abstract class BaseWorldHandler extends InjectionHandler {
    protected int randomWorldNumber() {
        return 1 + ThreadLocalRandom.current().nextInt(10000);
    }

    protected int parseQueryCount(Context ctx) {
        String textValue = ctx.getRequest().getQueryParams().get("queries");

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

    protected int[] getNumbers(int count) {
        int[] ids = new int[count];
        Arrays.setAll(ids, value -> randomWorldNumber());

        return ids;
    }
}
