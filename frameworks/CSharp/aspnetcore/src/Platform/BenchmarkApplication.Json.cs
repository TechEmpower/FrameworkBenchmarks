// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.IO.Pipelines;
using System.Text.Json;
using System.Threading.Tasks;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private readonly static uint _jsonPayloadSize = (uint)JsonSerializer.SerializeToUtf8Bytes(
            new JsonMessage { message = "Hello, World!" },
            SerializerContext.JsonMessage
            ).Length;

        private static ReadOnlySpan<byte> _jsonPreamble =>
            "HTTP/1.1 200 OK\r\n"u8 +
            "Server: K\r\n"u8 +
            "Content-Type: application/json\r\n"u8 +
            "Content-Length: 27"u8;


        private static Task Json(PipeWriter pipeWriter)
        {
            var writer = GetWriter(pipeWriter, sizeHint: 160);

            writer.Write(_jsonPreamble);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            writer.Commit();

            var utf8JsonWriter = t_writer ??= new Utf8JsonWriter(pipeWriter, new JsonWriterOptions { SkipValidation = true });
            utf8JsonWriter.Reset(pipeWriter);

            // Body
            JsonSerializer.Serialize(utf8JsonWriter, new JsonMessage { message = "Hello, World!" }, SerializerContext.JsonMessage);

            return Task.CompletedTask;
        }
    }
}
