using BeetleX;
using BeetleX.Buffers;
using System;
using System.Collections.Generic;
using System.Globalization;
using System.Text;
using System.Text.Encodings.Web;
using System.Text.Unicode;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {

        private readonly static AsciiString _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
        private readonly static AsciiString _fortunesRowStart = "<tr><td>";
        private readonly static AsciiString _fortunesColumn = "</td><td>";
        private readonly static AsciiString _fortunesRowEnd = "</td></tr>";
        private readonly static AsciiString _fortunesTableEnd = "</table></body></html>";

        [ThreadStatic]
        private static char[] mHtmlEncodeBuffer;

        protected HtmlEncoder HtmlEncoder { get; } = CreateHtmlEncoder();

        private static HtmlEncoder CreateHtmlEncoder()
        {
            var settings = new TextEncoderSettings(UnicodeRanges.BasicLatin, UnicodeRanges.Katakana, UnicodeRanges.Hiragana);
            settings.AllowCharacter('\u2014');  // allow EM DASH through
            return HtmlEncoder.Create(settings);
        }

        public async Task fortunes(PipeStream stream, HttpToken token, ISession session)
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
                    if (mHtmlEncodeBuffer == null)
                        mHtmlEncodeBuffer = new char[1024];
                    HtmlEncoder.Encode(item.Message, mHtmlEncodeBuffer, out int consumed, out int writtens);
                    //stream.Write(HtmlEncoder.Encode(item.Message));
                    stream.Write(new ArraySegment<char>(mHtmlEncodeBuffer, 0, writtens));
                    stream.Write(_fortunesRowEnd.Data, 0, _fortunesRowEnd.Length);
                }
                stream.Write(_fortunesTableEnd.Data, 0, _fortunesTableEnd.Length);
            }
            catch (Exception e_)
            {
                HttpServer.ApiServer.Log(BeetleX.EventArgs.LogType.Error, null, $"fortunes error {e_.Message}@{e_.StackTrace}");
                stream.Write(e_.Message);
            }
            OnCompleted(stream, session, token);
        }
    }
}
