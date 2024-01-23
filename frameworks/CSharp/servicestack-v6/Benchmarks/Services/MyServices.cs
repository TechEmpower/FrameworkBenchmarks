using ServiceStack;
using ServicestackV6.ServiceModel;

namespace ServicestackV6.ServiceInterface;

public class MyServices : Service
{
    private static readonly byte[] payload = System.Text.Encoding.UTF8.GetBytes("Hello, World!");

    public object Get(JsonRequest _)
    {
        Response.SetContentLength(27);
        return new JsonResponse();
    }

    public byte[] Get(PlainTextRequest _)
    {
        Response.SetContentLength(payload.Length);
        Response.ContentType = "text/plain";
        return payload;
    }
}