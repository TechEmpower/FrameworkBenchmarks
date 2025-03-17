using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using System.Text.Encodings.Web;
using System.Text.Json;
using System.Text.Unicode;
using System.Threading.Tasks;
using appMpower.Objects;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

namespace appMpower; 

public class FortunesMiddleware
{
    static readonly HtmlEncoder htmlEncoder = CreateHtmlEncoder();
    static HtmlEncoder CreateHtmlEncoder()
    {
        var settings = new TextEncoderSettings(UnicodeRanges.BasicLatin, UnicodeRanges.Katakana, UnicodeRanges.Hiragana);
        settings.AllowCharacter('\u2014'); // allow EM DASH through
        return HtmlEncoder.Create(settings);
    }

    private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", new StringValues("k"));
    private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", new StringValues("text/html; charset=UTF-8"));

    private static readonly byte[] _delimiter = new byte[] { 0xFF, 0xFF, 0xFF, 0xFF };

    private readonly RequestDelegate _next;

    public FortunesMiddleware(RequestDelegate next)
    {
        _next = next;
    }

    public unsafe Task Invoke(HttpContext httpContext)
    {
        if (httpContext.Request.Path.StartsWithSegments("/fortunes", StringComparison.Ordinal))
        {
            int payloadLength;
            IntPtr handlePointer; 

            IntPtr bytePointer = NativeMethods.Fortunes(out payloadLength, out handlePointer);

            /*
            byte[] json = new byte[payloadLength];
            Marshal.Copy(bytePointer, json, 0, payloadLength);
            NativeMethods.FreeHandlePointer(handlePointer);

            string s = Encoding.UTF8.GetString(json, 0, json.Length);

            var options = new JsonSerializerOptions
            {
                PropertyNameCaseInsensitive = true
            };

            List<Fortune> fortunes = JsonSerializer.Deserialize<List<Fortune>>(s, options);

            var response = httpContext.Response; 
            response.Headers.Add(_headerServer);

            var result = Results.Extensions.RazorSlice<Slices.Fortunes, List<Fortune>>(fortunes);
            result.HtmlEncoder = htmlEncoder;

            return result.ExecuteAsync(httpContext);
            */

            byte[] byteArray = new byte[payloadLength];
            Marshal.Copy(bytePointer, byteArray, 0, payloadLength);

            List<Fortune> fortunes = new List<Fortune>();

            // Convert the byte array into segments split by the delimiter
            int delimiterLength = _delimiter.Length;
            int start = 0;
            int index;

            while ((index = FindDelimiterIndex(byteArray, _delimiter, start)) >= 0)
            {
                // Use a span over the segment of bytes for the current object
                var objectDataSpan = new ReadOnlySpan<byte>(byteArray, start, index - start);
                Fortune fortune = ConvertBytesToObject(objectDataSpan);
                fortunes.Add(fortune);

                // Move past the delimiter
                start = index + delimiterLength;
            }

            NativeMethods.FreeHandlePointer(handlePointer);

            var response = httpContext.Response; 
            response.Headers.Add(_headerServer);

            var result = Results.Extensions.RazorSlice<Slices.Fortunes, List<Fortune>>(fortunes);
            result.HtmlEncoder = htmlEncoder;

            return result.ExecuteAsync(httpContext);

            /*
            var response = httpContext.Response; 
            response.Headers.Add(_headerServer);
            response.Headers.Add(_headerContentType);

            int payloadLength;
            IntPtr handlePointer; 

            IntPtr bytePointer = NativeMethods.Fortunes(out payloadLength, out handlePointer);
            byte[] json = new byte[payloadLength];
            Marshal.Copy(bytePointer, json, 0, payloadLength);
            NativeMethods.FreeHandlePointer(handlePointer);

            response.Headers.Add(
                new KeyValuePair<string, StringValues>("Content-Length", payloadLength.ToString()));

            return response.Body.WriteAsync(json, 0, payloadLength);
            */
        }

        return _next(httpContext);
    }

    private static int FindDelimiterIndex(byte[] array, byte[] delimiter, int startIndex)
    {
        int endIndex = array.Length - delimiter.Length;

        for (int i = startIndex; i <= endIndex; i++)
        {
            bool isMatch = true;

            for (int j = 0; j < delimiter.Length; j++)
            {
                if (array[i + j] != delimiter[j])
                {
                    isMatch = false;
                    break;
                }
            }

            if (isMatch)
            {
                return i;
            }
        }

        return -1;
    }

    private static Fortune ConvertBytesToObject(ReadOnlySpan<byte> data)
    {
        int offset = 0;

        // Read Id
        int id = BitConverter.ToInt32(data.Slice(offset, sizeof(int)));
        offset += sizeof(int);

        // Read Message (remaining bytes in the span)
        string message = Encoding.UTF8.GetString(data.Slice(offset));

        return new Fortune(id, message);
    }
}

public static class FortunesMiddlewareExtensions
{
    public static IApplicationBuilder UseFortunes(this IApplicationBuilder builder)
    {
        return builder.UseMiddleware<FortunesMiddleware>();
    }
}
