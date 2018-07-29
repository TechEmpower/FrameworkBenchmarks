// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Buffers.Text;
using System.IO.Pipelines;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Server.Kestrel.Core.Internal.Http;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication
    {
        private readonly static AsciiString _applicationName = "Kestrel Platform-Level Application";
        public static AsciiString ApplicationName => _applicationName;

        private readonly static AsciiString _crlf = "\r\n";
        private readonly static AsciiString _eoh = "\r\n\r\n"; // End Of Headers
        private readonly static AsciiString _http11OK = "HTTP/1.1 200 OK\r\n";
        private readonly static AsciiString _headerServer = "Server: Custom";
        private readonly static AsciiString _headerContentLength = "Content-Length: ";
        private readonly static AsciiString _headerContentLengthZero = "Content-Length: 0\r\n";
        private readonly static AsciiString _headerContentTypeText = "Content-Type: text/plain\r\n";
        private readonly static AsciiString _headerContentTypeJson = "Content-Type: application/json\r\n";
        private readonly static AsciiString _headerContentTypeHtml = "Content-Type: text/html; charset=UTF-8\r\n";

        private readonly static AsciiString _plainTextBody = "Hello, World!";

        private readonly static AsciiString _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
        private readonly static AsciiString _fortunesRowStart = "<tr><td>";
        private readonly static AsciiString _fortunesColumn = "</td><td>";
        private readonly static AsciiString _fortunesRowEnd = "</td></tr>";
        private readonly static AsciiString _fortunesTableEnd = "</table></body></html>";
        private readonly static AsciiString _contentLengthGap = new string(' ', 4);

        public static class Paths
        {
            public readonly static AsciiString SingleQuery = "/db";
            public readonly static AsciiString Json = "/json";
            public readonly static AsciiString Fortunes = "/fortunes";
            public readonly static AsciiString Plaintext = "/plaintext";
            public readonly static AsciiString Updates = "/updates/queries=";
        }

        private RequestType _requestType;
        private int _queries;

        public void OnStartLine(HttpMethod method, HttpVersion version, Span<byte> target, Span<byte> path, Span<byte> query, Span<byte> customMethod, bool pathEncoded)
        {
            var requestType = RequestType.NotRecognized;
            if (method == HttpMethod.Get)
            {
                var pathLength = path.Length;
                if (Paths.SingleQuery.Length <= pathLength && path.StartsWith(Paths.SingleQuery))
                {
                    requestType = RequestType.SingleQuery;
                }
                else if (Paths.Json.Length <= pathLength && path.StartsWith(Paths.Json))
                {
                    requestType = RequestType.Json;
                }
                else if (Paths.Fortunes.Length <= pathLength && path.StartsWith(Paths.Fortunes))
                {
                    requestType = RequestType.Fortunes;
                }
                else if (Paths.Plaintext.Length <= pathLength && path.StartsWith(Paths.Plaintext))
                {
                    requestType = RequestType.PlainText;
                }
                else if (Paths.Updates.Length <= pathLength && path.StartsWith(Paths.Updates))
                {
                    _queries = ParseQueries(path);
                    requestType = RequestType.Updates;
                }
            }

            _requestType = requestType;
        }

        private static int ParseQueries(Span<byte> path)
        {
            if (!Utf8Parser.TryParse(path.Slice(Paths.Updates.Length), out int queries, out _) || queries < 1)
            {
                queries = 1;
            }
            else if (queries > 500)
            {
                queries = 500;
            }

            return queries;
        }

        public Task ProcessRequestAsync()
        {
            var requestType = _requestType;
            var task = Task.CompletedTask;
            if (requestType == RequestType.PlainText)
            {
                PlainText(Writer);
            }
            else
            {
                Default(Writer);
            }

            return task;
        }

        private static void Default(PipeWriter pipeWriter)
        {
            var writer = GetWriter(pipeWriter);

            // HTTP 1.1 OK
            writer.Write(_http11OK);

            // Server headers
            writer.Write(_headerServer);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            // Content-Length 0
            writer.Write(_headerContentLengthZero);

            // End of headers
            writer.Write(_crlf);
            writer.Commit();
        }

        private enum RequestType
        {
            NotRecognized,
            PlainText,
            Json,
            Fortunes,
            SingleQuery,
            Updates
        }
    }
}
