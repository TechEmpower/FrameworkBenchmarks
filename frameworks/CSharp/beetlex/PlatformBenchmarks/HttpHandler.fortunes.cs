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

                stream.Write(_HtmlResultPreamble.Data, 0, _HtmlResultPreamble.Length);
                token.ContentLength = stream.Allocate(HttpHandler._LengthSize);
                GMTDate.Default.Write(stream);
                token.ContentPostion = stream.CacheLength;

                var html = token.GetHtmlBufferWriter();
                html.Reset();
                html.Write(_fortunesTableStart.Data, 0, _fortunesTableStart.Length);
                foreach (var item in data)
                {
                    html.Write(_fortunesRowStart.Data, 0, _fortunesRowStart.Length);
                    WriteNumeric(html, (uint)item.Id);
                    html.Write(_fortunesColumn.Data, 0, _fortunesColumn.Length);
                    html.Write(HtmlEncoder.Encode(item.Message));
                    html.Write(_fortunesRowEnd.Data, 0, _fortunesRowEnd.Length);
                }
                html.Write(_fortunesTableEnd.Data, 0, _fortunesTableEnd.Length);
                stream.Write(html.Data, 0, html.Length);

            }
            catch (Exception e_)
            {
                HttpServer.ApiServer.Log(BeetleX.EventArgs.LogType.Error, null, $"fortunes error {e_.Message}@{e_.StackTrace}");
                stream.Write(e_.Message);
            }
            OnCompleted(stream, session, token);
        }

        internal void WriteNumeric(HtmlBufferWriter writer, uint number)
        {
            const byte AsciiDigitStart = (byte)'0';

            if (number < 10)
            {
                writer.Write((byte)(number + AsciiDigitStart));

            }
            else if (number < 100)
            {
                var tens = (byte)((number * 205u) >> 11); // div10, valid to 1028
                var span = new byte[2];
                span[0] = (byte)(tens + AsciiDigitStart);
                span[1] = (byte)(number - (tens * 10) + AsciiDigitStart);
                writer.Write(span, 0, 2);

            }
            else if (number < 1000)
            {
                var digit0 = (byte)((number * 41u) >> 12); // div100, valid to 1098
                var digits01 = (byte)((number * 205u) >> 11); // div10, valid to 1028
                var span = new byte[3];
                span[0] = (byte)(digit0 + AsciiDigitStart);
                span[1] = (byte)(digits01 - (digit0 * 10) + AsciiDigitStart);
                span[2] = (byte)(number - (digits01 * 10) + AsciiDigitStart);
                writer.Write(span, 0, 3);
            }
        }

        internal void WriteNumeric(PipeStream stream, uint number)
        {
            const byte AsciiDigitStart = (byte)'0';

            if (number < 10)
            {
                stream.WriteByte((byte)(number + AsciiDigitStart));

            }
            else if (number < 100)
            {
                var tens = (byte)((number * 205u) >> 11); // div10, valid to 1028
                var span = new byte[2];
                span[0] = (byte)(tens + AsciiDigitStart);
                span[1] = (byte)(number - (tens * 10) + AsciiDigitStart);
                stream.Write(span, 0, 2);

            }
            else if (number < 1000)
            {
                var digit0 = (byte)((number * 41u) >> 12); // div100, valid to 1098
                var digits01 = (byte)((number * 205u) >> 11); // div10, valid to 1028
                var span = new byte[3];
                span[0] = (byte)(digit0 + AsciiDigitStart);
                span[1] = (byte)(digits01 - (digit0 * 10) + AsciiDigitStart);
                span[2] = (byte)(number - (digits01 * 10) + AsciiDigitStart);
                stream.Write(span, 0, 3);
            }
        }
    }
}
