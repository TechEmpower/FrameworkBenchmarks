using SpanJson;
using System;
using System.Threading.Tasks;
using ZYSocket;
using ZYSocket.FiberStream;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {

        public async void db(IFiberRw<HttpToken> fiberRw, WriteBytes write)
        {
            try
            {
                var data = await fiberRw.UserToken.Db.LoadSingleQueryRow();
                await JsonSerializer.NonGeneric.Utf8.SerializeAsync(data, write.Stream);
            }
            catch (Exception e_)
            {
                write.Write(e_.Message);               
            }

            OnCompleted(fiberRw, write);

        }
    }
}
