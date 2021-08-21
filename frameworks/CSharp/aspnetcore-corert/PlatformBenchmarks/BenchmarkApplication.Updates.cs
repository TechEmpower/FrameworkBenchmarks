// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Buffers;
using System.IO.Pipelines;
using System.Threading.Tasks;
using Utf8Json;

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
            var writer = GetWriter(pipeWriter, sizeHint: 120 * rows.Length); // in reality it's 112 for one

            writer.Write(_dbPreamble);

            var lengthWriter = writer;
            writer.Write(_contentLengthGap);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            writer.Commit();

            var jsonPayload = JsonSerializer.SerializeUnsafe(rows);
            pipeWriter.Write(jsonPayload);

            // Content-Length
            lengthWriter.WriteNumeric((uint)jsonPayload.Count);
        }
    }
}
