package hello.web;

import static spark.Spark.after;
import static spark.Spark.get;
import hello.domain.Message;
import hello.domain.World;
import hello.domain.Fortune;

import java.util.Date;
import java.util.Random;
import java.util.List;
import java.util.Collections;
import java.util.concurrent.ThreadLocalRandom;
import java.util.stream.Collectors;

import org.hibernate.Session;
import org.hibernate.Transaction;

import spark.Request;
import spark.Response;

import com.google.gson.Gson;

import static j2html.TagCreator.*;

public class SparkApplication implements spark.servlet.SparkApplication {

    private static final int DB_ROWS = 10000;
    private static final int FORTUNE_ROWS = 12;
    private static final String MESSAGE = "Hello, World!";
    private static final String ADDITIONAL_FORTUNE = "Additional fortune added at request time.";
    private static final String CONTENT_TYPE_JSON = "application/json";
    private static final String CONTENT_TYPE_TEXT = "text/plain";
    private static final Gson GSON = new Gson();

    private int getQueries(final Request request) {
      try {
        String param = request.queryParams("queries");
        if (param == null) {
          return 1;
        }

        int queries = Integer.parseInt(param);
        if (queries < 1) {
          return 1;
        }
        if (queries > 500) {
          return 500;
        }
        return queries;
      } catch (NumberFormatException ex) {
        return 1;
      }
    }

    @Override
    public void init() {

        get("/json", (request, response) -> {
          response.type(CONTENT_TYPE_JSON);
          return new Message(); }
        , GSON::toJson);
        get("/db", (request, response) -> {
            response.type(CONTENT_TYPE_JSON);

            final int queries = getQueries(request);

            final World[] worlds = new World[queries];
            final Session session = HibernateUtil.getSession();
            final Random random = ThreadLocalRandom.current();

            for (int i = 0; i < queries; i++) {
                worlds[i] = (World) session.byId(World.class).load(random.nextInt(DB_ROWS) + 1);
            }

            return (request.queryParams("queries") == null ? worlds[0] : worlds);
        }, GSON::toJson);
        get("/updates", (request, response) -> {
            response.type(CONTENT_TYPE_JSON);

            final int queries = getQueries(request);

            final World[] worlds = new World[queries];
            final Session session = HibernateUtil.getSession();
            final Random random = ThreadLocalRandom.current();

            for (int i = 0; i < queries; i++) {
                int id = random.nextInt(DB_ROWS) + 1;
                int randomNumber = random.nextInt(DB_ROWS) + 1;
                World world = (World) session.byId(World.class).load(id);
                world.randomNumber = randomNumber;
                Transaction transaction = session.beginTransaction();
                session.update(world);
                transaction.commit();
                worlds[i] = world;
            }

            return worlds;
        }, GSON::toJson);
        get("/plaintext", (request, response) -> {
            response.type(CONTENT_TYPE_TEXT);
            return MESSAGE;
        });
        get("/fortunes", (request, response) -> {
          final Session session = HibernateUtil.getSession();
          Fortune newFortune = new Fortune();
          newFortune.id = 0;
          newFortune.message = ADDITIONAL_FORTUNE;
          List<Fortune> fortunes = session.createCriteria(Fortune.class).list();
          fortunes.add(newFortune);
          Collections.sort(fortunes, (f1, f2) -> f1.message.compareTo(f2.message));
          return document().render() +
            html().with(
                head().with(
                    title("Fortunes")
                ),
                body().with(
                    table().with(
                        tr().with(
                            th("id"),
                            th("message")
                        )).with(
                        fortunes.stream().map((fortune) ->
                            tr().with(
                                td(Integer.toString(fortune.id)),
                                td(fortune.message)
                            )
                        ).collect(Collectors.toList())
                    )
                )
          ).render();

        });
        after((request, response) -> {
            HibernateUtil.closeSession();
            response.raw().addDateHeader("Date", new Date().getTime());
        });
    }

    public static void main(final String[] args) {
        System.setProperty("jndi", "false");
        new SparkApplication().init();
    }

}
