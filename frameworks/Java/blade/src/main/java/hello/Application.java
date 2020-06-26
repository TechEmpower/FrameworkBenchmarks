package hello;

import com.blade.Blade;
import com.blade.mvc.Const;
import com.blade.mvc.RouteContext;
import com.blade.mvc.http.StringBody;
import hello.model.Fortune;
import hello.model.Message;
import hello.model.World;

import java.util.List;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Stream;

import static io.github.biezhi.anima.Anima.select;
import static io.github.biezhi.anima.Anima.update;
import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.toList;

/**
 * Blade Application
 *
 * @author biezhi
 * @date 2018/10/17
 */
public class Application {

    private static final StringBody PLAINTEXT      = StringBody.of("Hello, World!");
    private static final String JSON_CONTENT_TYPE  = "application/json";
    private static final String SERVER_HEADER      = "Server";
    private static final String SERVER_VALUE       = "Blade-" + Const.VERSION;
    private static final String ADDITIONAL_FORTUNE = "Additional fortune added at request time.";

    private static final int DB_ROWS = 10000;

    private static int getQueries(String queries) {
        try {
            int count = Integer.valueOf(queries);
            return Math.min(500, Math.max(1, count));
        } catch (Exception e) {
            return 1;
        }
    }

    private static Integer generateId() {
        return 1 + ThreadLocalRandom.current().nextInt(DB_ROWS);
    }

    private static List<Integer> generateIdList(int size) {
        return Stream.iterate(0, num -> num + 1).limit(size)
                .map(i -> generateId())
                .collect(toList());
    }

    private static void db(RouteContext ctx) {
        World world = select().from(World.class).byId(generateId());
        ctx.json(world).contentType(JSON_CONTENT_TYPE).header(SERVER_HEADER, SERVER_VALUE);
    }

    private static void queries(RouteContext ctx) {
        int queries = getQueries(ctx.fromString("queries", "1"));

        List<Integer> idList = generateIdList(queries);

        List<World> worlds = idList.stream()
                .map(id -> select().from(World.class).byId(id))
                .collect(toList());
        ctx.json(worlds).contentType(JSON_CONTENT_TYPE).header(SERVER_HEADER, SERVER_VALUE);
    }

    private static void updates(RouteContext ctx) {
        int queries = getQueries(ctx.fromString("queries", "1"));

        List<Integer> idList = generateIdList(queries);

        List<World> worlds = idList.stream()
                .map(id -> select().from(World.class).byId(id))
                .peek(Application::updateWorld).collect(toList());

        ctx.json(worlds).contentType(JSON_CONTENT_TYPE).header(SERVER_HEADER, SERVER_VALUE);
    }

    private static void updateWorld(World world) {
        int number = generateId();

        update().from(World.class)
                .set("randomNumber", number)
                .where("id", world.getId())
                .execute();

        world.setRandomNumber(number);
    }

    private static void fortunes(RouteContext ctx) {
        List<Fortune> fortunes = select().from(Fortune.class).all();

        fortunes.add(new Fortune(0, ADDITIONAL_FORTUNE));
        fortunes.sort(comparing(Fortune::getMessage));

        ctx.attribute("fortunes", fortunes);
        ctx.header(SERVER_HEADER, SERVER_VALUE);
        ctx.render("fortunes.html");
    }

    public static void main(String[] args) {
        Blade.of()
                .get("/json", ctx -> ctx.json(new Message()).contentType(JSON_CONTENT_TYPE)
                        .header(SERVER_HEADER, SERVER_VALUE))
                .get("/plaintext", ctx -> ctx.body(PLAINTEXT).contentType("text/plain")
                        .header(SERVER_HEADER, SERVER_VALUE))
                .get("/db", Application::db)
                .get("/queries", Application::queries)
                .get("/updates", Application::updates)
                .get("/fortunes", Application::fortunes)
                .disableSession()
                .start(Application.class, args);
    }

}
