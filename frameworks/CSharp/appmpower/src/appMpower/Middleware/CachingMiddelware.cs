using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Primitives;

namespace appMpower;

public class CachingMiddleware
{
   //private static readonly MemoryCache _cache
   //   = new(new MemoryCacheOptions { ExpirationScanFrequency = TimeSpan.FromMinutes(60) });
   private static readonly Dictionary<int, byte[]> _cache = new();
   private static readonly Random _random = new();

   private static readonly byte[] _startBytes = Encoding.UTF8.GetBytes("[");
   private static readonly byte[] _endBytes = Encoding.UTF8.GetBytes("]");
   private static readonly byte[] _comma = Encoding.UTF8.GetBytes(",");


   private readonly static KeyValuePair<string, StringValues> _headerServer =
       new KeyValuePair<string, StringValues>("Server", new StringValues("k"));
   private readonly static KeyValuePair<string, StringValues> _headerContentType =
        new KeyValuePair<string, StringValues>("Content-Type", new StringValues("application/json"));

   private readonly RequestDelegate _next;

   public CachingMiddleware(RequestDelegate next)
   {
      _next = next;
   }

   public unsafe Task Invoke(HttpContext httpContext)
   {
      if (httpContext.Request.Path.StartsWithSegments("/cached-worlds", StringComparison.Ordinal))
      {
         int payloadLength;
         IntPtr handlePointer;
         IntPtr bytePointer;
         byte[] json;

         if (_cache.Count == 0)
         {
            for (int i = 1; i < 10001; i++)
            {
               bytePointer = NativeMethods.DbById(i, out payloadLength, out handlePointer);
               json = new byte[payloadLength];
               Marshal.Copy(bytePointer, json, 0, payloadLength);
               NativeMethods.FreeHandlePointer(handlePointer);
               //_cache.GetOrCreate<byte[]>(i, json);
               _cache.Add(i, json);
            }
         }

         var queryString = httpContext.Request.QueryString.ToString();
         int queries;
         Int32.TryParse(queryString.Substring(queryString.LastIndexOf("=") + 1), out queries);
         queries = queries > 500 ? 500 : (queries > 0 ? queries : 1);

         var response = httpContext.Response;
         response.Headers.Add(_headerServer);
         response.Headers.Add(_headerContentType);

         int queriesLength = 0;
         int[] keys = new int[queries];

         for (int i = 0; i < queries; i++)
         {
            keys[i] = _random.Next(1, 10001);
            queriesLength += _cache[keys[i]].Length;
         }

         byte[] result = new byte[_startBytes.Length + _endBytes.Length + (_comma.Length * queries - 1) + queriesLength];
         int position = 0;

         Buffer.BlockCopy(_startBytes, 0, result, position, _startBytes.Length);
         position += _startBytes.Length;

         for (int i = 0; i < queries; i++)
         {
            json = _cache[keys[i]];
            Buffer.BlockCopy(json, 0, result, position, json.Length);
            position += json.Length;

            if (i < queries - 1)
            {
               Buffer.BlockCopy(_comma, 0, result, position, _comma.Length);
               position += _comma.Length;
            }
         }

         Buffer.BlockCopy(_endBytes, 0, result, position, _endBytes.Length);

         response.Headers.Add(
             new KeyValuePair<string, StringValues>("Content-Length", result.Length.ToString()));

         return response.Body.WriteAsync(result, 0, result.Length);
      }

      return _next(httpContext);
   }
}

public static class CachingMiddlewareExtensions
{
   public static IApplicationBuilder UseCaching(this IApplicationBuilder builder)
   {
      return builder.UseMiddleware<CachingMiddleware>();
   }
}
