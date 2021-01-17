﻿using BeetleX;
using BeetleX.Buffers;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class HttpHandler
    {
        public Task Plaintext(PipeStream stream, HttpToken token, ISession session)
        {
            stream.Write(_result_plaintext.Data, 0, _result_plaintext.Length);
            OnCompleted(stream, session, token);
            return Task.CompletedTask;
        }
    }
}
