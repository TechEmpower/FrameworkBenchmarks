
using BeetleX.Light.Memory;
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

        static readonly HtmlEncoder htmlEncoder = CreateHtmlEncoder();
        static HtmlEncoder CreateHtmlEncoder()
        {
            var settings = new TextEncoderSettings(UnicodeRanges.BasicLatin, UnicodeRanges.Katakana, UnicodeRanges.Hiragana);
            settings.AllowCharacter('\u2014'); // allow EM DASH through
            return HtmlEncoder.Create(settings);
        }

        private static ReadOnlySpan<byte> _fortunesTableStart => "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>"u8;
        private static ReadOnlySpan<byte> _fortunesRowStart => "<tr><td>"u8;
        private static ReadOnlySpan<byte> _fortunesColumn => "</td><td>"u8;
        private static ReadOnlySpan<byte> _fortunesRowEnd => "</td></tr>"u8;
        private static ReadOnlySpan<byte> _fortunesTableEnd => "</table></body></html>"u8;


        public async Task fortunes(IStreamWriter stream)
        {
            ContentLengthMemory content = new ContentLengthMemory();
            try
            {

                var data = await this._db.LoadFortunesRows();

                stream.Write(_HtmlResultPreamble);
                content.Data = GetContentLengthMemory(stream);
                GMTDate.Default.Write(stream);

                stream.WriteSequenceNetStream.StartWriteLength();
                stream.Write(_fortunesTableStart);
                foreach (var item in data)
                {
                    stream.Write(_fortunesRowStart);
                    stream.WriteString(item.Id.ToString(CultureInfo.InvariantCulture));
                    stream.Write(_fortunesColumn);
                    stream.WriteString(htmlEncoder.Encode(item.Message));
                    stream.Write(_fortunesRowEnd);
                }
                stream.Write(_fortunesTableEnd);
            }
            catch (Exception e_)
            {
                Context.GetLoger(BeetleX.Light.Logs.LogLevel.Error)?.WriteException(Context, "PlatformBenchmarks", "fortunes", e_);
                stream.WriteString(e_.Message);
            }
            var len = stream.WriteSequenceNetStream.EndWriteLength();
            content.Full(len);

        }

        internal void WriteNumeric(IStreamWriter stream, uint number)
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
