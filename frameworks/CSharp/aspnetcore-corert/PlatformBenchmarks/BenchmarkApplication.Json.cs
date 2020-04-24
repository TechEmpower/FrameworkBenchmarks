// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using Utf8Json;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private static readonly uint _jsonPayloadSize = (uint)JsonSerializer.SerializeUnsafe(new JsonMessage { message = "Hello, World!" }).Count;

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

            var jsonPayload = JsonSerializer.SerializeUnsafe(new JsonMessage { message = "Hello, World!" });
            writer.Write(jsonPayload);
        }
    }
}
