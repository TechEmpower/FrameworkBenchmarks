using BeetleX.Light;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Sockets;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public class HttpNetApplication : ApplicationBase
    {
        public override bool Connecting(Socket socket, ListenHandler handler)
        {
            socket.NoDelay = true;
            return true;
        }
    }
}
