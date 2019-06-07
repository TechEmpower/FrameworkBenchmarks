package testTechempowerBaratine;

import static io.baratine.web.Web.*;
import io.baratine.web.WebServer;

public class Main
{
  public static void main(String[] args)
  {
    include(PlaintextService.class);
    include(JsonService.class);

    WebServer server = start();

    server.join();
  }
}
