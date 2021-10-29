using System;
using System.Collections.Generic;
using System.IO.Pipelines;
using System.Text.Json;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

namespace appMpower.Kestrel
{
   public static class Json
   {
      private readonly static KeyValuePair<string, StringValues> _headerServer =
         new KeyValuePair<string, StringValues>("Server", "k");
      private readonly static KeyValuePair<string, StringValues> _headerContentType =
         new KeyValuePair<string, StringValues>("Content-Type", "application/json");

      [ThreadStatic]
      private static Utf8JsonWriter _utf8JsonWriter;

      public static JsonWriterOptions _jsonWriterOptions = new JsonWriterOptions
      {
         SkipValidation = true
      };

      public static void RenderOne<T>(IHeaderDictionary headerDictionary, PipeWriter pipeWriter, T t, IJsonSerializer<T> jsonSerializer)
      {
         headerDictionary.Add(_headerServer);
         headerDictionary.Add(_headerContentType);

         Utf8JsonWriter utf8JsonWriter = _utf8JsonWriter ??= new Utf8JsonWriter(pipeWriter, new JsonWriterOptions { SkipValidation = true });
         utf8JsonWriter.Reset(pipeWriter);

         jsonSerializer.Serialize(utf8JsonWriter, t);
         utf8JsonWriter.Flush();
         headerDictionary.Add(new KeyValuePair<string, StringValues>("Content-Length", ((uint)utf8JsonWriter.BytesCommitted).ToString()));
      }

      public static void RenderMany<T>(IHeaderDictionary headerDictionary, PipeWriter pipeWriter, T[] tArray, IJsonSerializer<T> jsonSerializer)
      {
         headerDictionary.Add(_headerServer);
         headerDictionary.Add(_headerContentType);

         Utf8JsonWriter utf8JsonWriter = _utf8JsonWriter ??= new Utf8JsonWriter(pipeWriter, new JsonWriterOptions { SkipValidation = true });
         utf8JsonWriter.Reset(pipeWriter);

         utf8JsonWriter.WriteStartArray();

         foreach (var t in tArray)
         {
            jsonSerializer.Serialize(utf8JsonWriter, t);
         }

         utf8JsonWriter.WriteEndArray();
         utf8JsonWriter.Flush();
         headerDictionary.Add(new KeyValuePair<string, StringValues>("Content-Length", ((uint)utf8JsonWriter.BytesCommitted).ToString()));
      }
   }
}