package ro.pippo.benchmark.app;

import java.util.concurrent.ThreadLocalRandom;
import ro.pippo.benchmark.Benchmark;
import ro.pippo.core.ParameterValue;
import ro.pippo.core.route.RouteContext;

public final class BenchmarkUtils {

  public static final String HEADER_SERVER = "Server";
  public static final String HEADER_SERVER_VALUE = "Pippo " + Benchmark.SERVER_NAME;

  public static final String CONTENT_TYPE_TEXT_PLAIN = "text/plain";
  public static final String CONTENT_TYPE_JSON = "application/json";
  public static final String CONTENT_TYPE_HTML = "text/html";

  public static int random() {
    return 1 + ThreadLocalRandom.current().nextInt(10000);
  }

  public static int getQueriesParam(RouteContext routeContext) {
    ParameterValue param = routeContext.getParameter("queries");
    int queries = 1;
    try {
      if (!param.isEmpty()) {
        queries = Integer.parseInt(param.toString());
      }
    } catch (NumberFormatException e) {
      queries = 1;
    }
    if (queries < 1) {
      queries = 1;
    }
    if (queries > 500) {
      queries = 500;
    }
    return queries;
  }
}
