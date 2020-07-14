// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using Microsoft.AspNetCore.Server.Kestrel.Core.Internal.Http;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private readonly static AsciiString _applicationName = "Kestrel Platform-Level Application";
        public static AsciiString ApplicationName => _applicationName;

        private readonly static AsciiString _crlf = "\r\n";
        private readonly static AsciiString _http11OK = "HTTP/1.1 200 OK\r\n";
        private readonly static AsciiString _headerServer = "Server: K";
        private readonly static AsciiString _headerContentLength = "Content-Length: ";
        private readonly static AsciiString _headerContentLengthZero = "Content-Length: 0";
        private readonly static AsciiString _headerContentTypeText = "Content-Type: text/plain";
        private readonly static AsciiString _headerContentTypeJson = "Content-Type: application/json";

        private readonly static AsciiString _plainTextBody = "Hello, World!";

        public static class Paths
        {
            public readonly static AsciiString Json = "/json";
            public readonly static AsciiString Plaintext = "/plaintext";
        }

        private RequestType _requestType;

#if NETCOREAPP5_0 || NET5_0
        public void OnStartLine(HttpVersionAndMethod versionAndMethod, TargetOffsetPathLength targetPath, Span<byte> startLine)
        {
            var requestType = RequestType.NotRecognized;
            if (versionAndMethod.Method == HttpMethod.Get)
            {
                var pathLength = targetPath.Offset;
                if (pathLength == 10 && startLine.SequenceEqual(Paths.Plaintext))
                {
                    requestType = RequestType.PlainText;
                }
                else if (pathLength == 5 && startLine.SequenceEqual(Paths.Json))
                {
                    requestType = RequestType.Json;
                }
            }

            _requestType = requestType;
        }
#else
        public void OnStartLine(HttpMethod method, HttpVersion version, Span<byte> target, Span<byte> path, Span<byte> query, Span<byte> customMethod, bool pathEncoded)
        {
            var requestType = RequestType.NotRecognized;
            if (method == HttpMethod.Get)
            {
                if (path.Length == 10 && path.SequenceEqual(Paths.Plaintext))
                {
                    requestType = RequestType.PlainText;
                }
                else if (path.Length == 5 && path.SequenceEqual(Paths.Json))
                {
                    requestType = RequestType.Json;
                }
            }

            _requestType = requestType;
        }
#endif

        private void ProcessRequest(ref BufferWriter<WriterAdapter> writer)
        {
            if (_requestType == RequestType.PlainText)
            {
                PlainText(ref writer);
            }
            else if (_requestType == RequestType.Json)
            {
                Json(ref writer);
            }
            else
            {
                Default(ref writer);
            }
        }

        private readonly static AsciiString _defaultPreamble =
            _http11OK +
            _headerServer + _crlf +
            _headerContentTypeText + _crlf +
            _headerContentLengthZero;

        private static void Default(ref BufferWriter<WriterAdapter> writer)
        {
            writer.Write(_defaultPreamble);

            // Date header
            writer.Write(DateHeader.HeaderBytes);
        }

        private enum RequestType
        {
            NotRecognized,
            PlainText,
            Json
        }
    }
}
