using System.Web;
using System.Web.Script.Serialization;

public class JsonHandler : IHttpHandler
{
    bool IHttpHandler.IsReusable
    {
        get { return true; }
    }

    void IHttpHandler.ProcessRequest(HttpContext context)
    {
        HttpResponse response = context.Response;
        response.ContentType = "application/json";
        response.Write(new JavaScriptSerializer().Serialize(new { message = "Hello, World!" }));
    }
}