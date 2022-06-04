// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Buffers;
using System.Text.Json;

namespace PlatformBenchmarks;

public partial class BenchmarkApplication
{
    private readonly static uint _jsonPayloadSize = (uint)JsonSerializer.SerializeToUtf8Bytes(new JsonMessage { message = "Hello, World!" }, SerializerContext.JsonMessage).Length;

    private readonly static AsciiString _jsonPreamble =
        _http11OK +
        _headerServer + _crlf +
        _headerContentTypeJson + _crlf +
        _headerContentLength + _jsonPayloadSize.ToString();

    private static void Json(ref BufferWriter<WriterAdapter> writer, IBufferWriter<byte> bodyWriter)
    {
        writer.Write(_jsonPreamble);

        // Date header
        writer.Write(DateHeader.HeaderBytes);

        writer.Commit();

        Utf8JsonWriter utf8JsonWriter = t_writer ??= new Utf8JsonWriter(bodyWriter, new JsonWriterOptions { SkipValidation = true });
        utf8JsonWriter.Reset(bodyWriter);

        // Body
        JsonSerializer.Serialize(utf8JsonWriter, new JsonMessage { message = "Hello, World!" }, SerializerContext.JsonMessage);
    }
}
