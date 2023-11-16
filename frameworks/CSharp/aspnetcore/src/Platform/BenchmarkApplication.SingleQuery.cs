﻿// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.IO.Pipelines;
using System.Text.Json;
using System.Threading.Tasks;

namespace PlatformBenchmarks;

public partial class BenchmarkApplication
{
    private static async Task SingleQuery(PipeWriter pipeWriter)
    {
        OutputSingleQuery(pipeWriter, await RawDb.LoadSingleQueryRow());
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

        var utf8JsonWriter = t_writer ??= new Utf8JsonWriter(pipeWriter, new JsonWriterOptions { SkipValidation = true });
        utf8JsonWriter.Reset(pipeWriter);

        // Body
        JsonSerializer.Serialize(utf8JsonWriter, row, SerializerContext.World);

        // Content-Length
        lengthWriter.WriteNumeric((uint)utf8JsonWriter.BytesCommitted);
    }
}
