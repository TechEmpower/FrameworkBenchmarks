namespace NancyModules {

  using Nancy;

  public class PlainTextModule : NancyModule {

    public PlainTextModule() : base("/plaintext") 
    {
      Get["/"] = x =>
      {
        return "Hello, World!";
      };
    }

  }

}