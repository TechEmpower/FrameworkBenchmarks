package testTechempowerBaratine;

import static io.baratine.web.Web.*;

public class Main
{
  public static void main(String[] args)
  {
    include(PlaintextService.class);
    include(JsonService.class);

    start();

    join();
  }
}
