
using Swifter.Json;
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
                await JsonFormatter.SerializeObjectAsync(data, write.Stream, System.Text.Encoding.UTF8);
            }
            catch (Exception e_)
            {
                write.Write(e_.Message);               
            }

            await OnCompleted(fiberRw, write);

        }
    }
}
