import java.io.IOException;
import java.nio.charset.*;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;

import org.codehaus.jackson.map.ObjectMapper;
import org.vertx.java.core.Handler;
import org.vertx.java.core.eventbus.Message;
import org.vertx.java.core.http.HttpServerRequest;
import org.vertx.java.core.json.JsonArray;
import org.vertx.java.core.json.JsonObject;
import org.vertx.java.deploy.Verticle;

public class WebServer
	extends    Verticle
	implements Handler<HttpServerRequest>
{
  private final ObjectMapper mapper = new ObjectMapper();

  @Override
  public void start() throws Exception
  {
    this.getVertx().createHttpServer().requestHandler(this).listen(8080);
  }

  @Override
  public void handle(HttpServerRequest req)
  {
    if (req.path.equals("/json"))
    {
      handleJson(req);
    }
    else if (req.path.equals("/db"))
    {
      handleDb(req);
    }
    else
    {
      req.response.statusCode = 404;
      req.response.end();
    }
  }

  public static class HelloMessage
  {
    public final String message = "Hello, world";
  }

  private void handleJson(HttpServerRequest req)
  {
    String result;
    try
    {
      result = mapper.writeValueAsString(new HelloMessage());
    }
    catch (IOException e)
    {
      req.response.statusCode = 500;
      req.response.end();
      return;
    }
    
    int contentLength = result.getBytes(StandardCharsets.UTF_8).length;
    req.response.putHeader("Content-Type", "application/json; charset=UTF-8");
    req.response.putHeader("Content-Length", contentLength);
    req.response.write(result);
    req.response.end();
  }

  private void handleDb(final HttpServerRequest req)
  {
    int queriesParam = 1;
    try 
    {
      queriesParam = Integer.parseInt(req.params().get("queries"));
    }
    catch (NumberFormatException e)
    {
      // do nothing
    }

    final DbHandler dbh = new DbHandler(req, queriesParam);
    final Random random = ThreadLocalRandom.current();

    for (int i = 0; i < queriesParam; i++)
    {
      this.getVertx().eventBus().send(
        "hello.persistor",
        new JsonObject()
            .putString("action", "findone")
            .putString("collection", "world")
            .putObject("matcher", new JsonObject().putNumber("id", (random.nextInt(10000) + 1))),
        dbh);
    }
  }

  class DbHandler implements Handler<Message<JsonObject>>
  {
    private final HttpServerRequest req;
    private final int queries;
    private final List<Object> worlds = new CopyOnWriteArrayList<>();

    public DbHandler(HttpServerRequest request, int queriesParam)
    {
   	  this.req = request;
      this.queries = queriesParam;
    }

    @Override
    public void handle(Message<JsonObject> reply)
    {
      final JsonObject body = reply.body;

      if ("ok".equals(body.getString("status")))
      {
      	this.worlds.add(body.getObject("result"));
      }

      if (this.worlds.size() == this.queries)
      {
        // All queries have completed; send the response.
        // final JsonArray arr = new JsonArray(worlds);
        try
        {
          final String result = mapper.writeValueAsString(worlds);
          final int contentLength = result.getBytes(StandardCharsets.UTF_8).length;
          this.req.response.putHeader("Content-Type", "application/json; charset=UTF-8");
          this.req.response.putHeader("Content-Length", contentLength);
          this.req.response.write(result);
          this.req.response.end();
        }
        catch (IOException e)
        {
          req.response.statusCode = 500;
          req.response.end();
        }
      }
    }
  }
}
