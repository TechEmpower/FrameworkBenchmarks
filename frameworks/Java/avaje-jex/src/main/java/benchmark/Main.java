package benchmark;

import io.avaje.inject.BeanScope;
import io.avaje.jex.Jex;
import io.avaje.jex.Routing.HttpService;
import io.avaje.jsonb.Jsonb;
import io.avaje.jsonb.Types;
import java.util.Map;

public class Main {

  public static void main(String[] args) {

    var beans = BeanScope.builder().build();
    var routes = beans.list(HttpService.class);
    var type = Jsonb.builder().build().type(Types.mapOf(String.class));

    Jex.create()
        .config(c -> c.compression().disableCompression())
        .get("/plaintext", ctx -> ctx.text("Hello, World!"))
        .get("/json", ctx -> ctx.jsonb(type, Map.of("message", "Hello, World!")))
        .before(ctx -> ctx.header("Server", "avaje-jex"))
        .error(Exception.class, (ctx, _) -> ctx.status(503).text("503 Service Unavailable"))
        .routing(routes)
        .start();
  }
}
