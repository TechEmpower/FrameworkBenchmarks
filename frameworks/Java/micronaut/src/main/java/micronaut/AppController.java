package micronaut;

import com.github.mustachejava.Mustache;
import io.micronaut.http.HttpResponse;
import io.micronaut.http.MediaType;
import io.micronaut.http.annotation.Controller;
import io.micronaut.http.annotation.Get;
import io.micronaut.http.annotation.QueryValue;
import io.micronaut.spring.tx.annotation.Transactional;
import io.netty.util.AsciiString;
import io.reactivex.Single;
import org.hibernate.Session;
import org.hibernate.SessionFactory;

import java.io.StringWriter;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ThreadLocalRandom;

@Controller("/")
public class AppController {

    private static final CharSequence MESSAGE = AsciiString.cached("message");
    private static final CharSequence HELLO = AsciiString.cached("Hello, World!");

    private final SessionFactory sessionFactory;
    private final Mustache mustache;

    public AppController(SessionFactory sessionFactory, Mustache mustache) {
        this.sessionFactory = sessionFactory;
        this.mustache = mustache;
    }

    @Get(value = "/plaintext", produces = MediaType.TEXT_PLAIN)
    public Single<CharSequence> plaintext() {
        return Single.just(HELLO);
    }

    @Get(value = "/json", produces = MediaType.APPLICATION_JSON)
    public Single<Map<CharSequence, CharSequence>> json() {
        return Single.just(Collections.singletonMap(MESSAGE, HELLO.toString()));
    }

    @Get(value = "/db", produces = MediaType.APPLICATION_JSON)
    @Transactional(readOnly = true)
    public HttpResponse<World> db() {
        return HttpResponse.ok(findRandomWorld());
    }

    @Get(value = "/queries", produces = MediaType.APPLICATION_JSON)
    @Transactional(readOnly = true)
    public HttpResponse<World[]> queries(@QueryValue String queries) {
        return HttpResponse.ok(findRandomWorlds(parseQueryCount(queries)));
    }

    @Get(value = "/fortunes", produces = "text/html;charset=utf-8")
    @Transactional(readOnly = true)
    public HttpResponse<Writer> fortune() {
        StringWriter writer = new StringWriter();
        mustache.execute(writer, findFortunes());

        return HttpResponse.ok(writer);
    }

    @Get(value = "/updates", produces = MediaType.APPLICATION_JSON)
    @Transactional(readOnly = false)
    public HttpResponse<World[]> updates(@QueryValue String queries) {
        return HttpResponse.ok(updateRandomWorlds(parseQueryCount(queries)));
    }

    private World findRandomWorld() {
        return findRandomWorld(sessionFactory.getCurrentSession());
    }

    private World findRandomWorld(Session session) {
        return session.load(World.class, nextNumber());
    }

    private int nextNumber() {
        return ThreadLocalRandom.current().nextInt(10000) + 1;
    }

    private World[] findRandomWorlds(int count) {
        Session session = sessionFactory.getCurrentSession();

        World[] worlds = new World[count];
        for (int i = 0; i < count; i++) {
            worlds[i] = findRandomWorld(session);
        }

        return worlds;
    }

    private World[] updateRandomWorlds(int count) {
        Session session = sessionFactory.getCurrentSession();

        World[] worlds = new World[count];
        for (int i = 0; i < count; i++) {
            worlds[i] = findRandomWorld(session);
            worlds[i].setRandomNumber(nextNumber());
        }

        return worlds;
    }

    private List<Fortune> findFortunes() {
        List<Fortune> fortunes = sessionFactory.getCurrentSession().createQuery("from Fortune").list();

        List<Fortune> result = new ArrayList<Fortune>(fortunes.size() + 1);

        result.addAll(fortunes);
        result.add(new Fortune(0, "Additional fortune added at request time."));
        result.sort(Comparator.comparing(Fortune::getMessage));

        return result;
    }

    private int parseQueryCount(String textValue) {
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
