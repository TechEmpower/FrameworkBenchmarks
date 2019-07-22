using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using BeetleX.Buffers;
using BeetleX.FastHttpApi;

namespace Benchmarks
{
    public class FortuneView:BeetleX.FastHttpApi.ResultBase
    {
        private readonly static AsciiString _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
        private readonly static AsciiString _fortunesRowStart = "<tr><td>";
        private readonly static AsciiString _fortunesColumn = "</td><td>";
        private readonly static AsciiString _fortunesRowEnd = "</td></tr>";
        private readonly static AsciiString _fortunesTableEnd = "</table></body></html>";

        private static HeaderItem HTML_UTF8 = new HeaderItem("Content-Type: text/html; charset=UTF-8\r\n");

        public FortuneView(IEnumerable<Fortune> model)
        {
            Model = model;
        }

        public override bool HasBody => true;

        public IEnumerable<Fortune> Model { get; set; }

        public override IHeaderItem ContentType => HTML_UTF8;

        public override void Write(PipeStream stream, HttpResponse response)
        {
            stream.Write(_fortunesTableStart.Data, 0, _fortunesTableStart.Length);
            foreach (var item in Model)
            {
                stream.Write(_fortunesRowStart.Data, 0, _fortunesRowStart.Length);
                stream.Write(item.Id.ToString(CultureInfo.InvariantCulture));
                stream.Write(_fortunesColumn.Data, 0, _fortunesColumn.Length);
                stream.Write(System.Web.HttpUtility.HtmlEncode(item.Message));
                stream.Write(_fortunesRowEnd.Data, 0, _fortunesRowEnd.Length);
            }
            stream.Write(_fortunesTableEnd.Data, 0, _fortunesTableEnd.Length);
        }
    }

}
