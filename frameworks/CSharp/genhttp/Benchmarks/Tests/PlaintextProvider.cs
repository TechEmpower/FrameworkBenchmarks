using System;
using System.Collections.Generic;
using System.IO;
using System.Text;

using GenHTTP.Api.Modules;
using GenHTTP.Api.Protocol;

namespace Benchmarks.Tests
{

    public class PlaintextProvider : IContentProvider
    {
        private static readonly byte[] PAYLOAD = Encoding.ASCII.GetBytes("Hello, World!");

        public IResponseBuilder Handle(IRequest request)
        {
            return request.Respond()
                          .Content(new MemoryStream(PAYLOAD), ContentType.TextPlain);
        }

    }

}
