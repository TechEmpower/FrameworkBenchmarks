// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private static void PlainText(ref BufferWriter<WriterAdapter> writer)
        {
            // HTTP 1.1 OK
            writer.Write(_http11OK);

            // Server headers
            writer.Write(_headerServer);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            // Content-Type header
            writer.Write(_headerContentTypeText);

            // Content-Length header
            writer.Write(_headerContentLength);
            writer.WriteNumeric((uint)_plainTextBody.Length);

            // End of headers
            writer.Write(_eoh);

            // Body
            writer.Write(_plainTextBody);
        }
    }
}
