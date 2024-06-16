
using BeetleX.Light.Memory;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
      

        public void Plaintext(IStreamWriter stream)
        {
            stream.Write(_plaintextPreamble.Data, 0, _plaintextPreamble.Length);
            GMTDate.Default.Write(stream);
            stream.Write(_result_plaintext.Data, 0, _result_plaintext.Length);
        }
    }
}
