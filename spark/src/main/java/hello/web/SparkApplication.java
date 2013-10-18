package hello.web;

import static spark.Spark.after;
import static spark.Spark.get;
import hello.domain.Message;
import hello.domain.World;

import java.util.Date;
import java.util.Random;
import java.util.concurrent.ThreadLocalRandom;

import org.hibernate.Session;

import spark.Filter;
import spark.Request;
import spark.Response;
import spark.Route;

public class SparkApplication implements spark.servlet.SparkApplication {

    private static final int DB_ROWS = 10000;
    private static final String MESSAGE = "Hello, World!";
    private static final String CONTENT_TYPE_TEXT = "text/plain";
    
    @Override
    public void init() {
        get(new JsonTransformer("/json") {
            @Override
            protected Object handleInternal(final Request request, final Response response) {
                return new Message();
            }
        });
        get(new JsonTransformer("/db") {
            @Override
            protected Object handleInternal(final Request request, final Response response) {
                final int queries = getQueries(request);
                
                final World[] worlds = new World[queries];
                final Session session = HibernateUtil.getSession();
                final Random random = ThreadLocalRandom.current();
                
                for (int i = 0; i < queries; i++) {
                    worlds[i] = (World) session.byId(World.class).load(random.nextInt(DB_ROWS) + 1);
                }

                return worlds;
            }
            
            private int getQueries(final Request request) {
                String param = request.queryParams("queries");
                return (param == null ? 1 : Integer.parseInt(param));
            }
        });
        get(new Route("/plaintext") {
            @Override
            public Object handle(final Request request, final Response response) {
                response.type(CONTENT_TYPE_TEXT);
                return MESSAGE;
            }
        });
        after(new Filter("/db") {
            @Override
            public void handle(final Request request, final Response response) {
                HibernateUtil.closeSession();
            }
        });
        after(new Filter() {
            @Override
            public void handle(final Request request, final Response response) {
                response.raw().addDateHeader("Date", new Date().getTime());
            }
        });
    }
    
    public static void main(final String[] args) {
        System.setProperty("jndi", "false");
        new SparkApplication().init();
    }

}
