using AkazawaYun.PRO7;

namespace AkazawaYun.Benchmark.Platform;

class MyBenchmarkReceptor : akaWebReceptorBenchmark
{
    readonly JsonModel JsonModel;

    public MyBenchmarkReceptor()
    {
        JsonModel = new()
        {
            message = "Hello, World!"
        };
    }


    public override async ValueTask SendJson(IHttpContext http)
    {
        await http.Slient.Send(DataJson_OnlyHeader);
        akaJson.Text2Json(JsonModel, http.Slient.JsonSender);
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
