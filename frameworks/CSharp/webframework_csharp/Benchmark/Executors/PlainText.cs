using Framework;

public class PlainText : StatelessExecutor
{
    public override void DoGet(HttpRequest request, HttpResponse response)
    {
        response.AddHeader("Content-Type", "text/plain");

        response.SetBody("Hello, World!");
    }
}
