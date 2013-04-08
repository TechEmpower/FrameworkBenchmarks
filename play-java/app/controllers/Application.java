package controllers;

import akka.dispatch.Futures;
import models.World;
import play.api.libs.concurrent.Promise;
import play.libs.Akka;
import play.libs.F;
import play.libs.Json;

import static play.libs.Akka.future;

import org.codehaus.jackson.node.ObjectNode;
import org.codehaus.jackson.map.ObjectMapper;
import play.mvc.Controller;
import play.mvc.Result;
import scala.concurrent.ExecutionContext;

import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.Callable;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.TimeUnit;

public class Application extends Controller {

    private static final int TEST_DATABASE_ROWS = 10000;
    //http://stackoverflow.com/questions/3907929/should-i-make-jacksons-objectmapper-as-static-final
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    private static final ExecutionContext DB_EC = Akka.system().dispatchers().lookup("akka.actor.db");

    public static Result json() {
        final ObjectNode result = OBJECT_MAPPER.createObjectNode();
        result.put("message", "Hello World!");
        return ok(result);
    }

    public static Result db(final Integer queries) {
        return async(
                future(new Callable<Result>() {
                    @Override
                    public Result call() {
                        final Random random = ThreadLocalRandom.current();
                        final List<F.Promise<? extends World>> promises = new ArrayList<F.Promise<? extends World>>(queries);
                        for (int i = 0; i < queries; ++i) {
                            // There's no convenience method for submitting a future on an EC in Java. There is
                            // an issue that will address this though: https://github.com/playframework/Play20/issues/972
                            // Meanwhile we call the Akka future directly and wrap its result in a promise.
                            final F.Promise p = Akka.asPromise(Futures.future(
                                    findWorld(Long.valueOf(random.nextInt(TEST_DATABASE_ROWS) + 1)), DB_EC));
                            promises.add(p);
                        }
                        final List<World> worlds = F.Promise.sequence(promises).get(5L * queries, TimeUnit.SECONDS);
                        return ok(Json.toJson(worlds));
                    }

                    private Callable<World> findWorld(final Long id) {
                        return new Callable<World>() {
                            @Override
                            public World call() {
                                return World.find.byId(id);
                            }
                        };
                    }
                })
        );

    }
}
