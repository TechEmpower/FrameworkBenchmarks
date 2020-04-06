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

            writer.Write(_dbPreamble);

            // Content-Length
            var jsonPayload = JsonSerializer.SerializeToUtf8Bytes(rows, SerializerOptions);
            writer.WriteNumeric((uint)jsonPayload.Length);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            // Body
            writer.Write(jsonPayload);
            writer.Commit();
        }
    }
}
