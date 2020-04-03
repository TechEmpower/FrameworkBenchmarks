// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.IO.Pipelines;
using System.Text.Json;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private async Task Updates(PipeWriter pipeWriter, int count)
        {
            OutputUpdates(pipeWriter, await Db.LoadMultipleUpdatesRows(count));
        }

        private static void OutputUpdates(PipeWriter pipeWriter, World[] rows)
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
            var jsonPayload = JsonSerializer.SerializeToUtf8Bytes(rows, SerializerOptions);
            writer.WriteNumeric((uint)jsonPayload.Length);

            // End of headers
            writer.Write(_eoh);

            // Body
            writer.Write(jsonPayload);
            writer.Commit();
        }
    }
}
