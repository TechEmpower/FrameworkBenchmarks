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
        private readonly static AsciiString _headerContentLengthZero = "Content-Length: 0";
        private readonly static AsciiString _headerContentTypeText = "Content-Type: text/plain";
        private readonly static AsciiString _headerContentTypeJson = "Content-Type: application/json";
        private readonly static AsciiString _headerContentTypeHtml = "Content-Type: text/html; charset=UTF-8";

        private readonly static AsciiString _dbPreamble =
            _http11OK +
            _headerServer + _crlf +
            _headerContentTypeJson + _crlf +
            _headerContentLength;

        private readonly static AsciiString _plainTextBody = "Hello, World!";

        private static readonly JsonSerializerOptions SerializerOptions = new JsonSerializerOptions();

        private readonly static AsciiString _fortunesTableStart = "<!DOCTYPE html><html><head><title>Fortunes</title></head><body><table><tr><th>id</th><th>message</th></tr>";
        private readonly static AsciiString _fortunesRowStart = "<tr><td>";
        private readonly static AsciiString _fortunesColumn = "</td><td>";
        private readonly static AsciiString _fortunesRowEnd = "</td></tr>";
        private readonly static AsciiString _fortunesTableEnd = "</table></body></html>";
        private readonly static AsciiString _contentLengthGap = new string(' ', 4);

        public static RawDb Db { get; set; }

        [ThreadStatic]
        private static Utf8JsonWriter t_writer;

        public static class Paths
        {
            public readonly static AsciiString Json = "/json";
            public readonly static AsciiString Plaintext = "/plaintext";
            public readonly static AsciiString SingleQuery = "/db";
            public readonly static AsciiString Fortunes = "/fortunes";
            public readonly static AsciiString Updates = "/updates/";
            public readonly static AsciiString MultipleQueries = "/queries/";
        }

        private RequestType _requestType;
#if DATABASE
        private int _queries;
#endif

#if NETCOREAPP5_0 || NET5_0
        public void OnStartLine(HttpVersionAndMethod versionAndMethod, TargetOffsetPathLength targetPath, Span<byte> startLine)
        {
            var requestType = RequestType.NotRecognized;
            if (versionAndMethod.Method == HttpMethod.Get)
            {
#if !DATABASE
                var pathLength = targetPath.Offset;
                if (pathLength == 10 && startLine.SequenceEqual(Paths.Plaintext))
                {
                    requestType = RequestType.PlainText;
                }
                else if (pathLength == 5 && startLine.SequenceEqual(Paths.Json))
                {
                    requestType = RequestType.Json;
                }
#else
                var pathLength = targetPath.Offset;
                if (Paths.SingleQuery.Length == pathLength && startLine.SequenceEqual(Paths.SingleQuery))
                {
                    requestType = RequestType.SingleQuery;
                    return;
                }
                else if (Paths.Fortunes.Length == pathLength && startLine.SequenceEqual(Paths.Fortunes))
                {
                    requestType = RequestType.Fortunes;
                    return;
                }

                if (Paths.Updates.Length <= pathLength && startLine.StartsWith(Paths.Updates))
                {
                    _queries = ParseQueries(startLine, Paths.Updates.Length);
                    requestType = RequestType.Updates;
                }
                else if (Paths.MultipleQueries.Length <= pathLength && startLine.StartsWith(Paths.MultipleQueries))
                {
                    _queries = ParseQueries(startLine, Paths.MultipleQueries.Length);
                    requestType = RequestType.MultipleQueries;
                }
#endif
            }

            _requestType = requestType;
        }
#else
        public void OnStartLine(HttpMethod method, HttpVersion version, Span<byte> target, Span<byte> path, Span<byte> query, Span<byte> customMethod, bool pathEncoded)
        {
            var requestType = RequestType.NotRecognized;
            if (method == HttpMethod.Get)
            {
#if !DATABASE
                if (path.Length == 10 && path.SequenceEqual(Paths.Plaintext))
                {
                    requestType = RequestType.PlainText;
                }
                else if (path.Length == 5 && path.SequenceEqual(Paths.Json))
                {
                    requestType = RequestType.Json;
                }
#else
                var pathLength = path.Length; 
                if (Paths.Fortunes.Length == pathLength && path.SequenceEqual(Paths.Fortunes))
                {
                    requestType = RequestType.Fortunes;
                }
                else if (Paths.SingleQuery.Length == pathLength && path.SequenceEqual(Paths.SingleQuery))
                {
                    requestType = RequestType.SingleQuery;
                }
                else if (Paths.MultipleQueries.Length <= pathLength && path.StartsWith(Paths.MultipleQueries))
                {
                    _queries = ParseQueries(path, Paths.MultipleQueries.Length);
                    requestType = RequestType.MultipleQueries;
                }
                else if (Paths.Updates.Length <= pathLength && path.StartsWith(Paths.Updates))
                {
                    _queries = ParseQueries(path, Paths.Updates.Length);
                    requestType = RequestType.Updates;
                }
#endif
            }

            _requestType = requestType;
        }
#endif


#if !DATABASE
        private void ProcessRequest(ref BufferWriter<WriterAdapter> writer)
        {
            if (_requestType == RequestType.PlainText)
            {
                PlainText(ref writer);
            }
            else if (_requestType == RequestType.Json)
            {
                Json(ref writer, Writer);
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
            var writer = GetWriter(pipeWriter, sizeHint: _defaultPreamble.Length + DateHeader.HeaderBytes.Length);
            Default(ref writer);
            writer.Commit();
        }
#endif
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
            Json,
            Fortunes,
            SingleQuery,
            Updates,
            MultipleQueries
        }
    }
}
