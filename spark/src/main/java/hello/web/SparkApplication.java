package hello.web;

import static spark.Spark.after;
import static spark.Spark.get;
import hello.domain.Message;
import hello.domain.World;

import java.util.Random;

import org.hibernate.Session;

import spark.Filter;
import spark.Request;
import spark.Response;
import spark.Route;

import com.google.gson.Gson;

public class SparkApplication implements spark.servlet.SparkApplication {

    private static final Gson   GSON                 = new Gson();
    private static final Random RANDOM               = new Random();
    private static final int    DB_ROWS              = 10000;
    private static final String CONTENT_TYPE_JSON    = "application/json";
    
    @Override
    public void init() {
        get(new Route("/json") {
            @Override
            public Object handle(final Request request, final Response response) {
                response.type(CONTENT_TYPE_JSON);
                return GSON.toJson(new Message());
            }
        });
        get(new Route("/db") {
            @Override
            public Object handle(final Request request, final Response response) {
                response.type(CONTENT_TYPE_JSON);
                int queries = getQueries(request);
                
                World[] worlds = new World[queries];
                Session session = HibernateUtil.getSession();
                
                for (int i = 0; i < queries; i++) {
                    worlds[i] = (World) session.byId(World.class).load(RANDOM.nextInt(DB_ROWS) + 1);
                }

                return GSON.toJson(worlds);
            }
            
            private int getQueries(final Request request) {
                String param = request.queryParams("queries");
                return (param == null ? 1 : Integer.parseInt(param));
            }
        });
        after(new Filter("/db") {
            @Override
            public void handle(final Request request, final Response response) {
                HibernateUtil.closeSession();
            }
        });
    }
    
    public static void main(final String[] args) {
        System.setProperty("jndi", "false");
        new SparkApplication().init();
    }

}
