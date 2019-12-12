using Swifter.Json;
using System;
using ZYSocket;
using ZYSocket.FiberStream;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public async void queries(string queryString, IFiberRw<HttpToken> fiberRw, WriteBytes write)
        {
            int count = 1;
            if (!string.IsNullOrEmpty(queryString))
            {
                var values = queryString.Split('=');
                if (values.Length > 1)
                {
                    if (int.TryParse(values[1], out int size))
                    {
                        count = size;
                    }
                }
            }
            if (count > 500)
                count = 500;
            if (count < 1)
                count = 1;
            try
            {
                var data = await fiberRw.UserToken.Db.LoadMultipleQueriesRows(count);               

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
