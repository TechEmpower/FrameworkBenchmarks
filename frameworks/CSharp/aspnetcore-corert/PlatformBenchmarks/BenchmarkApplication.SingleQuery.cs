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
        private async Task SingleQuery(PipeWriter pipeWriter)
        {
            OutputSingleQuery(pipeWriter, await Db.LoadSingleQueryRow());
        }

        private static void OutputSingleQuery(PipeWriter pipeWriter, World row)
        {
            var writer = GetWriter(pipeWriter, sizeHint: 180); // in reality it's 150

            writer.Write(_dbPreamble);

            var lengthWriter = writer;
            writer.Write(_contentLengthGap);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            writer.Commit();

            var jsonPayload = JsonSerializer.SerializeUnsafe(row);
            pipeWriter.Write(jsonPayload);

            // Content-Length
            lengthWriter.WriteNumeric((uint)jsonPayload.Count);
        }
    }
}
