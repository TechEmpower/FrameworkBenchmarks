using System;
using System.Collections.Generic;
using System.Globalization;
using System.Threading.Tasks;
using ZYSocket;
using ZYSocket.FiberStream;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {

        private readonly static AsciiString _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";     
        private readonly static AsciiString _fortunesRowStart = "<tr><td>";
        private readonly static AsciiString _fortunesColumn = "</td><td>";
        private readonly static AsciiString _fortunesRowEnd = "</td></tr>";
        private readonly static AsciiString _fortunesTableEnd = "</table></body></html>";

        public async void fortunes(IFiberRw<HttpToken> fiberRw, WriteBytes write)
        {
         

            try
            {
                var data = await fiberRw.UserToken.Db.LoadFortunesRows();

                Task<int> WSend()
                {
                    write.Write(_fortunesTableStart.Data, 0, _fortunesTableStart.Length);
                    foreach (var item in data)
                    {
                        write.Write(_fortunesRowStart.Data, 0, _fortunesRowStart.Length);
                        write.Write(item.Id.ToString(CultureInfo.InvariantCulture),false);
                        write.Write(_fortunesColumn.Data, 0, _fortunesColumn.Length);
                        write.Write(System.Web.HttpUtility.HtmlEncode(item.Message),false);
                        write.Write(_fortunesRowEnd.Data, 0, _fortunesRowEnd.Length);
                    }
                    write.Write(_fortunesTableEnd.Data, 0, _fortunesTableEnd.Length);

                    var length = write.Stream.Length - fiberRw.UserToken.HttpHandlerPostion;
                    write.Stream.Position = fiberRw.UserToken.ContentPostion.postion;
                    write.Write(length.ToString(), false);
                    write.Flush(false);
                    return fiberRw.Flush();
                }
                if (fiberRw.UserToken != null)
                    await await fiberRw.Sync.Ask(WSend);
            }
            catch (Exception e_)
            {
                write.Write(e_.Message);
                await OnCompleted(fiberRw, write);
            }


        }
    }
}
