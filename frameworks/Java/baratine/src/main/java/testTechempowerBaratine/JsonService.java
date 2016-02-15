package testTechempowerBaratine;

import io.baratine.service.Result;
import io.baratine.service.Service;
import io.baratine.web.http.Get;

@Service
public class JsonService
{
  @Get("/json")
  public void hello(Result<HelloWorld> result)
  {
    result.ok(new HelloWorld("Hello, World!"));
  }

  public static class HelloWorld {
    private String message;

    public HelloWorld(String msg)
    {
      this.message = msg;
    }
  }
}
