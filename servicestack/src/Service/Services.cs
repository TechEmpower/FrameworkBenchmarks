using System;
using System.Collections.Generic;
using System.Linq;

using ServiceStack.CacheAccess;
using ServiceStack.CacheAccess.Providers;
using ServiceStack.CacheAccess.Memcached;
using ServiceStack.CacheAccess.AwsDynamoDb;
using ServiceStack.CacheAccess.Azure;
using ServiceStack.Common.Web;
using ServiceStack.Redis;
using ServiceStack.ServiceHost;
using ServiceStack.ServiceInterface;

namespace ServiceStackBenchmark
{

    #region Hello World Services

    [Api("Test #1 (JSON serialization) using Service Stack")]
    [Route("/json", "GET")]
    public class JsonRequest { }

    public class JsonService : Service
    {
        public object Get(JsonRequest request)
        {
            var response = new { message = "Hello, World!" };
            return response;
        }
    }

    [Api("Test #6 (Plaintext) using Service Stack")]
    [Route("/plaintext", "GET")]
    public class PlainTextRequest { }

    public class PlainTextService : Service
    {
        public object Get(PlainTextRequest request)
        {
            var response = new HttpResult("Hello, World!", "text/plain");
            return response;
        }
    }


    [Api("Set Cache Provider")]
    [Route("/cacheprovider/{provider}", "GET")]
    public class SetCacheProviderRequest 
    {
        [ApiMember(Name ="provider", Description = "Cache Provider", DataType = "string", IsRequired = true)]
        [ApiAllowableValues("provider", new string[] { "inmem", "memcache", "redis", "aws", "azure" })]
        public string provider { get; set; }    
    }

    public class CacheProviderService : Service
    {
        public object Any(SetCacheProviderRequest request)
        {
            try
            {
                switch (request.provider)
                {
                    case "memcache":
                        var memcache = new MemcachedClientCache();
                        AppHost.Instance.Container.Register<ICacheClient>(memcache);
                        return new HttpResult("Cache Provider switched to MemCache.");

                    case "redis":
                        AppHost.Instance.Container.Register<ICacheClient>(c => c.Resolve<IRedisClientsManager>().GetCacheClient());
                        return new HttpResult("Cache Provider switched to Redis.");

                    case "aws":
                        var aws = new DynamoDbCacheClient("", "", Amazon.RegionEndpoint.APSoutheast1);
                        AppHost.Instance.Container.Register<ICacheClient>(aws);
                        return new HttpResult("Cache Provider switched to Amazon Web Service DynamoDb Cache Client.");

                    case "azure":
                        AppHost.Instance.Container.Register<ICacheClient>(new AzureCacheClient("default"));
                        return new HttpResult("Cache Provider switched to Microsoft Azure Cache Client.");

                    default:
                        AppHost.Instance.Container.Register<ICacheClient>(new MemoryCacheClient());
                        return new HttpResult("Cache Provider switched to In-Memory Cache Client.");
                }
            }
            catch
            {
                throw;
            }
        }
    }


    #endregion



}