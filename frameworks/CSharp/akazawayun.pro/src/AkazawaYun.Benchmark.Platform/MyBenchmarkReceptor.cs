using AkazawaYun.PRO7;
using System.Text;

namespace AkazawaYun.Benchmark.Platform;

class MyBenchmarkReceptor : akzWebReceptorBenchmark
{
    readonly JsonModel JsonModel;
    readonly ReadOnlyMemory<byte> JsonContentLength;


    public MyBenchmarkReceptor()
    {
        JsonModel = new()
        {
            message = "Hello, World!"
        };
        akzJson.Text2Json(JsonModel, out ReadOnlyMemory<byte> json);
        JsonContentLength = Encoding.UTF8.GetBytes($"{json.Length}\r\n\r\n");
    }


    public override ValueTask SendPlaintext(IHttpContext http)
    {
        return base.SendPlaintext(http);
    }
    public override async ValueTask SendJson(IHttpContext http)
    {
        await http.Slient.Send(DataJson_OnlyHeaderExceptContentLength);
        await http.Slient.Send(JsonContentLength);
        akzJson.Text2Json(JsonModel, out ReadOnlyMemory<byte> json);
        await http.Slient.Send(json);
    }
    public override ValueTask SendDb(IHttpContext http)
    {
        throw new NotImplementedException();
    }
    public override ValueTask SendQueries(IHttpContext http)
    {
        throw new NotImplementedException();
    }
    public override ValueTask SendUpdates(IHttpContext http)
    {
        throw new NotImplementedException();
    }
    public override ValueTask SendCachedQueries(IHttpContext http)
    {
        throw new NotImplementedException();
    }
    public override ValueTask SendFortunes(IHttpContext http)
    {
        throw new NotImplementedException();
    }

}
