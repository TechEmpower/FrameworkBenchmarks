using System;
using System.Threading.Tasks;
using System.Collections.Generic;
using System.IO.Pipelines;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;
using Microsoft.AspNetCore.Http.Features;

namespace appMpower.Kestrel
{
   public static class PlainText
   {
      private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", new StringValues("k"));
      private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", new StringValues("text/plain"));

      public static async Task RenderAsync(IHeaderDictionary headerDictionary, PipeWriter pipeWriter, ReadOnlyMemory<byte> utf8String)
      {
         headerDictionary.Add(_headerServer);
         headerDictionary.Add(_headerContentType);
         headerDictionary.Add(new KeyValuePair<string, StringValues>("Content-Length", utf8String.Length.ToString()));

         await pipeWriter.WriteAsync(utf8String);
         pipeWriter.Complete();
      }

      public static void Render(IHeaderDictionary headerDictionary, IHttpResponseBodyFeature httpResponseBodyFeature, byte[] utf8String)
      {
         headerDictionary.Add(_headerServer);
         headerDictionary.Add(_headerContentType);
         int length = utf8String.Length;
         headerDictionary.Add(new KeyValuePair<string, StringValues>("Content-Length", length.ToString()));

         httpResponseBodyFeature.Stream.Write(utf8String, 0, length);
      }
   }
}