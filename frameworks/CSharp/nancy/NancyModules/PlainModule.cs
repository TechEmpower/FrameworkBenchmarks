namespace NancyModules 
{
  using Nancy;

  public class PlainModule : NancyModule
  {
    public PlainModule() : base("plaintext")
    {
      Get["/"] = x =>
      {
        return "Hello, World!";
      };
    }
  }
}