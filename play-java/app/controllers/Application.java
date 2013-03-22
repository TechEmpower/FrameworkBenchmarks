package controllers;

import play.*;
import play.mvc.*;
import play.libs.Json;
import org.codehaus.jackson.node.ObjectNode;
import views.html.*;
import models.*;
import java.util.*;
import java.util.concurrent.*;

public class Application extends Controller {

  private static final int TEST_DATABASE_ROWS = 10000;
  
  public static Result json() {
    ObjectNode result = Json.newObject();
    result.put("message", "Hello World!");
    return ok(result);
  }

  public static Result db(Integer queries) {
    final Random random = ThreadLocalRandom.current();
    final World[] worlds = new World[queries];

    for (int i = 0; i < queries; i++)
    {
      worlds[i] = World.find.byId((long)(random.nextInt(TEST_DATABASE_ROWS) + 1));
    }

    return ok(Json.toJson(worlds));
  }

}
