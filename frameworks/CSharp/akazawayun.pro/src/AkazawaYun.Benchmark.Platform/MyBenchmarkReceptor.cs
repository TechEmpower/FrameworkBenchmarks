using AkazawaYun.PRO7;

namespace AkazawaYun.Benchmark.Platform;

class MyBenchmarkReceptor : akzWebReceptorBenchmark
{
    readonly JsonModel JsonModel;


    public MyBenchmarkReceptor()
    {
        JsonModel = new()
        {
            message = "Hello, World!"
        };
    }


    public override ValueTask SendPlaintext(IHttpContext http)
    {
        return base.SendPlaintext(http);
    }
    public override async ValueTask SendJson(IHttpContext http)
    {
        await http.Slient.Send(DataJson_OnlyHeader);
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
