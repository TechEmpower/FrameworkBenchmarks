// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Buffers.Text;
using System.IO.Pipelines;
using System.Text.Json;
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
        private readonly static AsciiString _headerServer = "Server: K";
        private readonly static AsciiString _headerContentLength = "Content-Length: ";
        private readonly static AsciiString _headerContentLengthZero = "Content-Length: 0\r\n";
        private readonly static AsciiString _headerContentTypeText = "Content-Type: text/plain\r\n";
        private readonly static AsciiString _headerContentTypeJson = "Content-Type: application/json\r\n";
        private readonly static AsciiString _headerContentTypeHtml = "Content-Type: text/html; charset=UTF-8\r\n";

        private readonly static AsciiString _plainTextBody = "Hello, World!";

        private static readonly JsonSerializerOptions SerializerOptions = new JsonSerializerOptions();

        private readonly static AsciiString _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
        private readonly static AsciiString _fortunesRowStart = "<tr><td>";
        private readonly static AsciiString _fortunesColumn = "</td><td>";
        private readonly static AsciiString _fortunesRowEnd = "</td></tr>";
        private readonly static AsciiString _fortunesTableEnd = "</table></body></html>";
        private readonly static AsciiString _contentLengthGap = new string(' ', 4);

        public static RawDb Db { get; set; }

        public static class Paths
        {
            public readonly static AsciiString SingleQuery = "/db";
            public readonly static AsciiString Json = "/j";
            public readonly static AsciiString Fortunes = "/fortunes";
            public readonly static AsciiString Plaintext = "/p";
            public readonly static AsciiString Updates = "/updates/queries=";
            public readonly static AsciiString MultipleQueries = "/queries/queries=";
        }

        private RequestType _requestType;
#if DATABASE
        private int _queries;
#endif
        public void OnStartLine(HttpMethod method, HttpVersion version, Span<byte> target, Span<byte> path, Span<byte> query, Span<byte> customMethod, bool pathEncoded)
        {
            var requestType = RequestType.NotRecognized;
            if (method == HttpMethod.Get)
            {
#if !DATABASE
                if (path.Length >= 2 && path[0] == '/')
                {
                    if (path[1] == 'j')
                    {
                        requestType = RequestType.Json;
                    }
                    else if (path[1] == 'p')
                    {
                        requestType = RequestType.PlainText;
                    }
                }
#else
                var pathLength = path.Length;
                if (Paths.SingleQuery.Length <= pathLength && path.StartsWith(Paths.SingleQuery))
                {
                    requestType = RequestType.SingleQuery;
                }
                else if (Paths.Fortunes.Length <= pathLength && path.StartsWith(Paths.Fortunes))
                {
                    requestType = RequestType.Fortunes;
                }
                else if (Paths.Updates.Length <= pathLength && path.StartsWith(Paths.Updates))
                {
                    _queries = ParseQueries(path, Paths.Updates.Length);
                    requestType = RequestType.Updates;
                }
                else if (Paths.MultipleQueries.Length <= pathLength && path.StartsWith(Paths.MultipleQueries))
                {
                    _queries = ParseQueries(path, Paths.MultipleQueries.Length);
                    requestType = RequestType.MultipleQueries;
                }
#endif
            }

            _requestType = requestType;
        }


#if !DATABASE
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
#else
        private static int ParseQueries(Span<byte> path, int pathLength)
        {
            if (!Utf8Parser.TryParse(path.Slice(pathLength), out int queries, out _) || queries < 1)
            {
                queries = 1;
            }
            else if (queries > 500)
            {
                queries = 500;
            }

            return queries;
        }

        private Task ProcessRequestAsync()
        {
            Task task;
            var requestType = _requestType;
            if (requestType == RequestType.Fortunes)
            {
                task = Fortunes(Writer);
            }
            else if (requestType == RequestType.SingleQuery)
            {
                task = SingleQuery(Writer);
            }
            else if (requestType == RequestType.Updates)
            {
                task = Updates(Writer, _queries);
            }
            else if (requestType == RequestType.MultipleQueries)
            {
                task = MultipleQueries(Writer, _queries);
            }
            else
            {
                Default(Writer);
                task = Task.CompletedTask;
            }

            return task;
        }

        private static void Default(PipeWriter pipeWriter)
        {
            var writer = GetWriter(pipeWriter);
            Default(ref writer);
            writer.Commit();
        }
#endif
        private static void Default(ref BufferWriter<WriterAdapter> writer)
        {
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
        }

        private enum RequestType
        {
            NotRecognized,
            PlainText,
            Json,
            Fortunes,
            SingleQuery,
            Updates,
            MultipleQueries
        }
    }
}
