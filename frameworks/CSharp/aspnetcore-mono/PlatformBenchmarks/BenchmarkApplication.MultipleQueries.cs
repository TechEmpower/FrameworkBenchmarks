﻿// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.IO.Pipelines;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Server.Kestrel.Core.Internal.Http;
using Utf8Json;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private async Task MultipleQueries(PipeWriter pipeWriter, int count)
        {
            OutputMultipleQueries(pipeWriter, await Db.LoadMultipleQueriesRows(count));
        }

        private static void OutputMultipleQueries(PipeWriter pipeWriter, World[] rows)
        {
            var writer = GetWriter(pipeWriter);

            // HTTP 1.1 OK
            writer.Write(_http11OK);

            // Server headers
            writer.Write(_headerServer);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            // Content-Type header
            writer.Write(_headerContentTypeJson);

            // Content-Length header
            writer.Write(_headerContentLength);
            var jsonPayload = JsonSerializer.SerializeUnsafe(rows);
            writer.WriteNumeric((uint)jsonPayload.Count);

            // End of headers
            writer.Write(_eoh);

            // Body
            writer.Write(jsonPayload);
            writer.Commit();
        }
    }
}
