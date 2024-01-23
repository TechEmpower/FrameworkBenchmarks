using System;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Microsoft.Extensions.Logging.Abstractions;

namespace appMpower.Kestrel
{
   public class ServiceProvider : ISupportRequiredService, IServiceProvider
   {
      public object GetRequiredService(Type serviceType)
      {
         return GetService(serviceType);
      }

      public object GetService(Type serviceType)
      {
         if (serviceType == typeof(ILoggerFactory))
         {
            return NullLoggerFactory.Instance;
         }

         return null;
      }
   }
}