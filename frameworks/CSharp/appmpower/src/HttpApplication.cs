using System;
using System.Text;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Hosting.Server;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Features;
using appMpower.Kestrel;

namespace appMpower
{
   public class HttpApplication : IHttpApplication<IFeatureCollection>
   {
      public static readonly byte[] _plainText = Encoding.UTF8.GetBytes("Hello, World!");
      private readonly static JsonMessageSerializer _jsonMessageSerializer = new JsonMessageSerializer();
      private readonly static WorldSerializer _worldSerializer = new WorldSerializer();

      public IFeatureCollection CreateContext(IFeatureCollection featureCollection)
      {
         return featureCollection;
      }

      public async Task ProcessRequestAsync(IFeatureCollection featureCollection)
      {
         var request = featureCollection as IHttpRequestFeature;
         var httpResponse = featureCollection as IHttpResponseFeature;
         var httpResponseBody = featureCollection as IHttpResponseBodyFeature;

         PathString pathString = request.Path;

         if (pathString.HasValue)
         {
            int pathStringLength = pathString.Value.Length;
            string pathStringStart = pathString.Value.Substring(1, 1);

            if (pathStringLength == 10 && pathStringStart == "p")
            {
               await PlainText.RenderAsync(httpResponse.Headers, httpResponseBody.Writer, _plainText);
               return;
            }
            else if (pathStringLength == 5 && pathStringStart == "j")
            {
               Json.RenderOne(httpResponse.Headers, httpResponseBody.Writer, new JsonMessage { message = "Hello, World!" }, _jsonMessageSerializer);
               return;
            }
            else if (pathStringLength == 3 && pathStringStart == "d")
            {
               Json.RenderOne(httpResponse.Headers, httpResponseBody.Writer, await RawDb.LoadSingleQueryRow(), _worldSerializer);
               return;
            }
            else if (pathStringLength == 8 && pathStringStart == "q")
            {
               int count = 1;

               if (!Int32.TryParse(request.QueryString.Substring(request.QueryString.LastIndexOf("=") + 1), out count) || count < 1)
               {
                  count = 1;
               }
               else if (count > 500)
               {
                  count = 500;
               }

               //Json.RenderMany(httpResponse.Headers, httpResponseBody.Writer, await RawDb.LoadMultipleQueriesRows(count), _worldSerializer);
               Json.RenderMany(httpResponse.Headers, httpResponseBody.Writer, await RawDb.ReadMultipleRows(count), _worldSerializer);
               return;
            }
            else if (pathStringLength == 9 && pathStringStart == "f")
            {
               FortunesView.Render(httpResponse.Headers, httpResponseBody.Writer, await RawDb.LoadFortunesRows());
               return;
            }
            else if (pathStringLength == 8 && pathStringStart == "u")
            {
               int count = 1;

               if (!Int32.TryParse(request.QueryString.Substring(request.QueryString.LastIndexOf("=") + 1), out count) || count < 1)
               {
                  count = 1;
               }
               else if (count > 500)
               {
                  count = 500;
               }

               Json.RenderMany(httpResponse.Headers, httpResponseBody.Writer, await RawDb.LoadMultipleUpdatesRows(count), _worldSerializer);
               return;
            }
         }
      }

      public void DisposeContext(IFeatureCollection featureCollection, Exception exception)
      {
      }
   }
}