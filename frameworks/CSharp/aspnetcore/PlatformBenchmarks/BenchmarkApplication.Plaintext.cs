// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace PlatformBenchmarks;

public partial class BenchmarkApplication
{
    private static ReadOnlySpan<byte> _plaintextPreamble =>
        "HTTP/1.1 200 OK\r\n"u8 +
        "Server: K\r\n"u8 +
        "Content-Type: text/plain\r\n"u8 +
        "Content-Length: 13"u8;

    private static void PlainText(ref BufferWriter<WriterAdapter> writer)
    {
        writer.Write(_plaintextPreamble);

        // Date header
        writer.Write(DateHeader.HeaderBytes);

        // Body
        writer.Write(_plainTextBody);
    }
}
