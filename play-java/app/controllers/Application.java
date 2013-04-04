package controllers;

import play.libs.Json;
import play.mvc.*;
import static play.libs.Akka.future;
import java.util.concurrent.Callable;
import org.codehaus.jackson.node.ObjectNode;
import org.codehaus.jackson.map.ObjectMapper;
import views.html.*;
import models.*;
import java.util.*;
import java.util.concurrent.*;

public class Application extends Controller {

    private static final int TEST_DATABASE_ROWS = 10000;
    //http://stackoverflow.com/questions/3907929/should-i-make-jacksons-objectmapper-as-static-final
    private static ObjectMapper objectMapper = new ObjectMapper();

    public static Result json() {
        return async(
          future(new Callable<Result>() {
              public Result call() {
                  ObjectNode result = objectMapper.createObjectNode();
                  result.put("message", "Hello World!");
                  return ok(result);
              }
          })
        );
    }

    public static Result db(final Integer queries) {
        return async(
          future(new Callable<Result>() {
              public Result call() {
                  final Random random = ThreadLocalRandom.current();
                  final World[] worlds = new World[queries];

                  for (int i = 0; i < queries; i++) {
                      worlds[i] = World.find.byId((long)(random.nextInt(TEST_DATABASE_ROWS) + 1));
                  }
                  return ok(Json.toJson(worlds));
              }
          })
        );

    }
}
