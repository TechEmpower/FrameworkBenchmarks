package controllers;

import play.*;
import play.mvc.*;
import play.libs.Json;

import views.html.*;
import models.*;
import java.util.*;
import java.util.concurrent.*;
import com.fasterxml.jackson.databind.*;

public class Application extends Controller {

  private static final int TEST_DATABASE_ROWS = 10000;
  private static final ObjectMapper mapper = new ObjectMapper();
  
  public static Result json() throws Exception {
    Map<String, String> data = new HashMap<String, String>();
    data.put("message", "Hello, world");
    return ok(mapper.writeValueAsString(data)).as("application/json");
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
