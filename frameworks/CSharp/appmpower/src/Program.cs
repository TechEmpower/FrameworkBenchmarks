using System;
using System.Net;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Hosting.Server.Features;
using Microsoft.AspNetCore.Server.Kestrel.Core;
using Microsoft.AspNetCore.Server.Kestrel.Transport.Sockets;
using Microsoft.Extensions.Logging.Abstractions;
using Microsoft.Extensions.Options;

namespace appMpower
{
   class Program
   {
      static async Task Main(string[] args)
      {
         var socketTransportOptions = new SocketTransportOptions();
         var socketTransportFactory = new SocketTransportFactory(Options.Create(socketTransportOptions), NullLoggerFactory.Instance);
         var kestrelServerOptions = new KestrelServerOptions();

         kestrelServerOptions.Listen(IPAddress.Any, 8080);
         kestrelServerOptions.AddServerHeader = false;

         using var kestrelServer = new KestrelServer(Options.Create(kestrelServerOptions), socketTransportFactory, NullLoggerFactory.Instance);

         await kestrelServer.StartAsync(new HttpApplication(), CancellationToken.None);

         Console.WriteLine("Listening on:");

         foreach (var address in kestrelServer.Features.Get<IServerAddressesFeature>().Addresses)
         {
            Console.WriteLine(" - " + address);
         }

         Console.WriteLine("Process CTRL+C to quit");
         var wh = new ManualResetEventSlim();
         Console.CancelKeyPress += (sender, e) => wh.Set();
         wh.Wait();
      }
   }
}