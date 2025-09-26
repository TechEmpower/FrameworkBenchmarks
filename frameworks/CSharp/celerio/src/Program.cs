using System.Net;
using Celerio.Generated;

var server = new Server(IPAddress.Any, 8080);
server.Start();
await Task.Delay(Timeout.Infinite);