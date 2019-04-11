package controllers;

import static models.tables.Fortune.FORTUNE;
import static models.tables.World.WORLD;
import static play.mvc.Http.MimeTypes.JSON;

import java.util.Collections;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import javax.inject.Inject;

import org.jooq.DSLContext;
import org.jooq.JSONFormat;
import org.jooq.JSONFormat.RecordFormat;
import org.jooq.SQLDialect;
import org.jooq.impl.DSL;
import org.jooq.types.UInteger;

import models.tables.records.FortuneRecord;
import models.tables.records.WorldRecord;
import play.db.Database;
import play.mvc.Controller;
import play.mvc.Result;

public class Application extends Controller {

    private static final SQLDialect DIALECT = SQLDialect.MYSQL_5_7;
    private static final JSONFormat JSON_FORMAT = JSONFormat.DEFAULT_FOR_RECORDS.recordFormat(RecordFormat.OBJECT);

    private final Database db;

    @Inject
    public Application(final Database db) {
        this.db = db;
    }

    public Result db() {
        return ok(getRandomWorlds(1).get(0).formatJSON(JSON_FORMAT)).as(JSON);
    }

    public Result queries(final String queries) {
        return ok(getRandomWorlds(queryCount(queries)).formatJSON(JSON_FORMAT)).as(JSON);
    }

    public Result fortunes() {
        final List<FortuneRecord> fortunes = this.db.withConnection(connection -> {
            return DSL.using(connection, DIALECT).select(FORTUNE.ID, FORTUNE.MESSAGE).from(FORTUNE).fetchInto(FortuneRecord.class);
        });
        fortunes.add(new FortuneRecord(UInteger.valueOf(0), "Additional fortune added at request time."));
        Collections.sort(fortunes, (f1, f2) -> f1.getMessage().compareTo(f2.getMessage()));

        return ok(views.html.fortunes.render(fortunes));
    }

    public Result update(final String queries) {
        final org.jooq.Result<WorldRecord> worlds =  getRandomWorlds(queryCount(queries));

        final Random random = ThreadLocalRandom.current();
        for (final WorldRecord world : worlds) {
            world.setRandomnumber((random.nextInt(10000) + 1));
        }

        final int batchSize = 25;
        final int batches = ((worlds.size() / batchSize) + 1);
        this.db.withConnection(connection -> {
            final DSLContext sql = DSL.using(connection, DIALECT);
            for ( int i = 0 ; i < batches ; ++i ) {
                sql.batchUpdate(worlds.subList(i * batchSize, Math.min((i + 1) * batchSize, worlds.size()))).execute();
            }
            return null;
        });

        return ok(worlds.formatJSON(JSON_FORMAT)).as(JSON);
    }

    private int queryCount(final String queryCountString) {
        int queryCount;
        try {
            queryCount = Integer.parseInt(queryCountString, 10);
        } catch (final NumberFormatException e) {
            queryCount = 1;
        }
        if (queryCount < 1) {
            queryCount = 1;
        } else if (queryCount > 500) {
            queryCount = 500;
        }

        return queryCount;
    }

    private org.jooq.Result<WorldRecord> getRandomWorlds(final int n) {
        final Random random = ThreadLocalRandom.current();
        org.jooq.Result<WorldRecord> worlds = null;
        for (int i = 0; i < n; ++i) {
            long randomId = random.nextInt(10000) + 1;
            final org.jooq.Result<WorldRecord> world = this.db.withConnection(connection -> {
                return DSL.using(connection, DIALECT).selectFrom(WORLD).where(WORLD.ID.eq(UInteger.valueOf(randomId))).fetch();
            });

            if(worlds == null) {
                worlds = world;
            } else {
                worlds.add(world.get(0));
            }
        }
        return worlds;
    }

}
