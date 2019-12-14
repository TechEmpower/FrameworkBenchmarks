using BeetleX;
using BeetleX.Buffers;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {

        private readonly static AsciiString _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
        private readonly static AsciiString _fortunesRowStart = "<tr><td>";
        private readonly static AsciiString _fortunesColumn = "</td><td>";
        private readonly static AsciiString _fortunesRowEnd = "</td></tr>";
        private readonly static AsciiString _fortunesTableEnd = "</table></body></html>";

        public async void fortunes(PipeStream stream, HttpToken token, ISession session)
        {
            try
            {
                var data = await token.Db.LoadFortunesRows();
                stream.Write(_fortunesTableStart.Data, 0, _fortunesTableStart.Length);
                foreach (var item in data)
                {
                    stream.Write(_fortunesRowStart.Data, 0, _fortunesRowStart.Length);
                    stream.Write(item.Id.ToString(CultureInfo.InvariantCulture));
                    stream.Write(_fortunesColumn.Data, 0, _fortunesColumn.Length);
                    stream.Write(System.Web.HttpUtility.HtmlEncode(item.Message));
                    stream.Write(_fortunesRowEnd.Data, 0, _fortunesRowEnd.Length);
                }
                stream.Write(_fortunesTableEnd.Data, 0, _fortunesTableEnd.Length);
            }
            catch (Exception e_)
            {
                stream.Write(e_.Message);
            }
            OnCompleted(stream, session, token);
        }
    }
}
