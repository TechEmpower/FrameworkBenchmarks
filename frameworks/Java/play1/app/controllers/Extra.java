package controllers;

import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.ExecutionException;

import models.Fortune;
import models.World;
import play.Logger;
import play.db.jpa.JPAPlugin;
import play.jobs.Job;
import play.libs.F.Promise;
import play.mvc.Controller;

public class Extra extends Controller {

    private static final int TEST_DATABASE_ROWS = 10000;
    private static final int TEST_FORTUNE_ROWS = 100;

    // FIXME: should this test be consistent - ie set seed or not?
    private static Random random = new Random();
    private static int fortuneId;

    @play.db.jpa.NoTransaction
    public static void setupTx() {
        JPAPlugin plugin = play.Play.plugin(JPAPlugin.class);
        plugin.startTx(true);

        // clean out the old
        World.deleteAll();
        System.out.println("DELETED");
        // in with the new
        for (long i = 0; i <= TEST_DATABASE_ROWS; i++) {
            int randomNumber = random.nextInt(TEST_DATABASE_ROWS) + 1;
            new World(i, randomNumber).save();
            if (i % 100 == 0) {

                World.em().flush();
                World.em().clear();
                System.out.println("FLUSHED : " + i + "/" + TEST_DATABASE_ROWS);

            }
        }
        System.out.println("ADDED");
        plugin.closeTx(false);
    }

    final private static char[] hexArray = "0123456789ABCDEF".toCharArray();

    private static String bytesToHex(byte[] bytes) {
        char[] hexChars = new char[bytes.length * 2];
        int v;
        for (int j = 0; j < bytes.length; j++) {
            v = bytes[j] & 0xFF;
            hexChars[j * 2] = hexArray[v >>> 4];
            hexChars[j * 2 + 1] = hexArray[v & 0x0F];
        }
        return new String(hexChars);
    }

    // EXTRA
    public static void createWorlds() {
        final int MAXROWS = 100;
        World[] items = new World[MAXROWS];
        for (int i = 0; i < MAXROWS; i++) {
            int randomNumber = random.nextInt(MAXROWS) + 1;
            World item = new World();
            item.id = new Long(i);
            item.randomNumber = new Long(randomNumber);
            items[i] = item;
        }
        renderJSON(items);
    }

    // EXTRA
    public static void setupFortuneHex() {

        // clean out the old
        World.deleteAll();
        System.out.println("(setup) Worlds deleted");
        // in with the new
        for (long i = 0; i <= TEST_DATABASE_ROWS; i++) {
            int randomNumber = random.nextInt(TEST_DATABASE_ROWS) + 1;
            new World(i, randomNumber).save();
            if (i % 100 == 0) {

                World.em().flush();
                World.em().clear();
                System.out.println("(setup) Worlds added : " + i + "/"
                        + TEST_DATABASE_ROWS);

            }

        }
        System.out.println("(setup) Worlds created");

        Fortune.deleteAll();
        System.out.println("(setup) Fortunes deleted");
        for (int i = 0; i <= TEST_FORTUNE_ROWS; i++) {
            int randomNumber = random.nextInt(TEST_FORTUNE_ROWS) + 1;

            byte[] bytes = ByteBuffer.allocate(4).putInt(randomNumber).array();
            byte[] hash;
            try {
                hash = MessageDigest.getInstance("MD5").digest(bytes);
                String message = bytesToHex(hash);
                new Fortune(i, message).save();

                if (i % 100 == 0) {

                    Fortune.em().flush();
                    Fortune.em().clear();
                    System.out.println("(setup) Fortunes added : " + i + "/"
                            + TEST_FORTUNE_ROWS);

                }

            } catch (NoSuchAlgorithmException e) {
                Logger.fatal("Can't get MD5 digest", e);
                e.printStackTrace();
            }

        }
        System.out.println("(setup) Fortunes created");
    }

    public static void setup() {

        // clean out the old
        World.deleteAll();
        System.out.println("(setup) Worlds deleted");
        // in with the new
        for (long i = 0; i <= TEST_DATABASE_ROWS; i++) {
            int randomNumber = random.nextInt(TEST_DATABASE_ROWS) + 1;
            new World(i, randomNumber).save();
            if (i % 100 == 0) {

                World.em().flush();
                World.em().clear();
                System.out.println("(setup) Worlds added : " + i + "/"
                        + TEST_DATABASE_ROWS);

            }

        }
        System.out.println("(setup) Worlds created");

        Fortune.deleteAll();
        System.out.println("(setup) Fortunes deleted");
        fortuneId = 0;
        createFortune("fortune: No such file or directory");
        createFortune("A computer scientist is someone who fixes things that aren't broken.");
        createFortune("After enough decimal places, nobody gives a damn.");
        createFortune("A bad random number generator: 1, 1, 1, 1, 1, 4.33e+67, 1, 1, 1");
        createFortune("A computer program does what you tell it to do, not what you want it to do.");
        createFortune("Emacs is a nice operating system, but I prefer UNIX. — Tom Christaensen");
        createFortune("Any program that runs right is obsolete.");
        createFortune("A list is only as strong as its weakest link. — Donald Knuth");
        createFortune("Feature: A bug with seniority.");
        createFortune("Computers make very fast, very accurate mistakes.");
        createFortune("<script>alert(\"This should not be displayed in a browser alert box.\");</script>");
        createFortune("フレームワークのベンチマーク");


        System.out.println("(setup) Fortunes created");
    }

    private static void createFortune(String string) {
        new Fortune(fortuneId, string).save();
        fortuneId++;
        Fortune.em().flush();
        Fortune.em().clear();

    }

    /**
     * EXTRA: note this is method is much slower than the synchronous version
     */
    public static void dbAsyncEachQuery(int queries)
            throws InterruptedException, ExecutionException {
        if (queries == 0)
            queries = 1;
        final int queryCount = queries;
        List<Promise<World>> promises = new ArrayList<Promise<World>>();
        for (int i = 0; i < queryCount; ++i) {
            final Long id = Long
                    .valueOf(random.nextInt(TEST_DATABASE_ROWS) + 1);
            Job<World> job = new Job<World>() {
                public World doJobWithResult() throws Exception {
                    World result = World.findById(id);
                    return result;
                };
            };
            promises.add(job.now());
        }
        List<World> result = await(Promise.waitAll(promises));
        renderJSON(result);
    }
}