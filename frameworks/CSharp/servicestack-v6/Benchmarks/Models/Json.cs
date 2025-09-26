using ServiceStack;

namespace ServicestackV6.ServiceModel;

[Route("/json")]
public class JsonRequest : IReturn<JsonResponse> { }

public class JsonResponse
{
    public string message => "Hello, World!";
}