using AkazawaYun.PRO7;

namespace AkazawaYun.Benchmark.Platform;

class MyBenchmarkReceptor : akaWebReceptorBenchmark
{
    readonly JsonModel JsonModel;
    readonly ReadOnlyMemory<byte> OnlyUsedForTestNonAllocJson;

    public MyBenchmarkReceptor()
    {
        JsonModel = new()
        {
            message = "Hello, World!"
        };
        akaJson.Text2Json(JsonModel, out OnlyUsedForTestNonAllocJson);
    }


    public override async ValueTask SendJson(IHttpContext http)
    {
        await http.Slient.Send(DataJson_OnlyHeader);

        //akaJson.Text2Json(JsonModel, out ReadOnlyMemory<byte> json);
        //await http.Slient.Send(json);

        // [WARN_2025.12.21] this code is temporaryly not creating new json for every request, only for test if use fixed json data to simulate non-alloc new byte-array !!!
        await http.Slient.Send(OnlyUsedForTestNonAllocJson);
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
