package controllers;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Random;

import models.Fortune;
import models.World;
import play.mvc.Controller;

public class Application extends Controller {

    private static final int TEST_DATABASE_ROWS = 10000;

    // FIXME: should this test be consistent - ie set seed or not?
    private static Random random = new Random();

    public static void index() {
        render();
    }

    public static void hello() {
        renderText("hello world");
    }

    public static void plaintext() {
        renderText("hello world");
    }

    public static void json() {
        Map<String, String> result = new HashMap<String, String>();
        result.put("message", "Hello World!");
        renderJSON(result);
    }

    public static void db() {
        Long id = Long.valueOf(random.nextInt(TEST_DATABASE_ROWS) + 1);
        World result = World.findById(id);
        renderJSON(result);
    }

    public static void queries(int queries) {
        if (queries == 0)
            queries = 1;
        if (queries > 500)
            queries = 500;
        final List<World> worlds = new ArrayList<World>();
        for (int i = 0; i < queries; ++i) {
            Long id = Long.valueOf(random.nextInt(TEST_DATABASE_ROWS) + 1);
            World result = World.findById(id);
            worlds.add(result);
        }
        renderJSON(worlds);
    }

    public static void updates(int queries) {
        // Bounds check.
        if (queries > 500) {
            queries = 500;
        }
        if (queries < 1) {
            queries = 1;
        }
        final World[] worlds = new World[queries];

        // Run the query the number of times requested.
        for (int i = 0; i < queries; i++) {
            final long id = random.nextInt(TEST_DATABASE_ROWS) + 1;
            World item = World.findById(id);
            worlds[i] = item;
            item.randomNumber = new Long(random.nextInt(TEST_DATABASE_ROWS) + 1);
            item.save();
        }
        renderJSON(worlds);
    }

    public static void fortunes() {
        List<Fortune> fortunes = Fortune.findAll();
        fortunes.add(new Fortune(0, "Additional fortune added at request time."));
        Collections.sort(fortunes);
        render(fortunes);
    }

}