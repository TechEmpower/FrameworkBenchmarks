// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Text.Json;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private readonly static uint _jsonPayloadSize = (uint)JsonSerializer.SerializeToUtf8Bytes(new JsonMessage { message = "Hello, World!" }, SerializerOptions).Length;

        private readonly static AsciiString _jsonPreamble =
            _http11OK +
            _headerServer + _crlf +
            _headerContentTypeJson + _crlf +
            _headerContentLength + _jsonPayloadSize.ToString();

        private static void Json(ref BufferWriter<WriterAdapter> writer)
        {
            writer.Write(_jsonPreamble);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            writer.Commit();

            // Body
            using (Utf8JsonWriter utf8JsonWriter = new Utf8JsonWriter(writer.Output))
            {
                JsonSerializer.Serialize<JsonMessage>(utf8JsonWriter, new JsonMessage { message = "Hello, World!" }, SerializerOptions);
            }
        }
    }
}
