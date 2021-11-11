package testTechempowerBaratine;

import io.baratine.service.Result;
import io.baratine.service.Service;
import io.baratine.web.Get;

@Service
public class PlaintextService
{
  @Get("/plaintext")
  public void hello(Result<String> result)
  {
    result.ok("Hello, World!");
  }
}
