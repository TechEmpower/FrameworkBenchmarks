using CodeBehind;

public partial class PlainTextController : CodeBehindController
{
    public void PageLoad(HttpContext context)
    {
        context.Response.ContentType = "text/plain";

        Write("Hello, World!");
    }
}