using Framework;

public class Json : StatelessExecutor
{
    public override void DoGet(HttpRequest request, HttpResponse response)
    {
        var message = new
        {
            message = "Hello, World!"
        };

        response.SetBody(message);
    }
}
