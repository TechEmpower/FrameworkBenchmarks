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
        private readonly static AsciiString _http11NotFound = "HTTP/1.1 404 Not Found\r\n";
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

#if DATABASE
        public static RawDb Db { get; set; }
#endif

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
            public readonly static AsciiString Caching = "/cached-worlds/";
        }

        private RequestType _requestType;
        private int _queries;

#if NET5_0
        public void OnStartLine(HttpVersionAndMethod versionAndMethod, TargetOffsetPathLength targetPath, Span<byte> startLine)
        {
            _requestType = versionAndMethod.Method == HttpMethod.Get ? GetRequestType(startLine.Slice(targetPath.Offset, targetPath.Length), ref _queries) : RequestType.NotRecognized;
        }
#else
        public void OnStartLine(HttpMethod method, HttpVersion version, Span<byte> target, Span<byte> path, Span<byte> query, Span<byte> customMethod, bool pathEncoded)
        {
            _requestType = method == HttpMethod.Get ? GetRequestType(path, ref _queries) : RequestType.NotRecognized;
        }
#endif

        private RequestType GetRequestType(ReadOnlySpan<byte> path, ref int queries)
        {
#if !DATABASE
            if (path.Length == 10 && path.SequenceEqual(Paths.Plaintext))
            {
                return RequestType.PlainText;
            }
            else if (path.Length == 5 && path.SequenceEqual(Paths.Json))
            {
                return RequestType.Json;
            }
#else
            if (path.Length == 3 && path[0] == '/' && path[1] == 'd' && path[2] == 'b')
            {
                return RequestType.SingleQuery;
            }
            else if (path.Length == 9 && path[1] == 'f' && path.SequenceEqual(Paths.Fortunes))
            {
                return RequestType.Fortunes;
            }
            else if (path.Length >= 15 && path[1] == 'c' && path.StartsWith(Paths.Caching))
            {
                queries = ParseQueries(path.Slice(15));
                return RequestType.Caching;
            }
            else if (path.Length >= 9 && path[1] == 'u' && path.StartsWith(Paths.Updates))
            {
                queries = ParseQueries(path.Slice(9));
                return RequestType.Updates;
            }
            else if (path.Length >= 9 && path[1] == 'q' && path.StartsWith(Paths.MultipleQueries))
            {
                queries = ParseQueries(path.Slice(9));
                return RequestType.MultipleQueries;
            }
#endif
            return RequestType.NotRecognized;
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
                Json(ref writer, Writer);
            }
            else
            {
                Default(ref writer);
            }
        }
#else

        private static int ParseQueries(ReadOnlySpan<byte> parameter)
        {
            if (!Utf8Parser.TryParse(parameter, out int queries, out _) || queries < 1)
            {
                queries = 1;
            }
            else if (queries > 500)
            {
                queries = 500;
            }

            return queries;
        }

        private Task ProcessRequestAsync() => _requestType switch
        {
            RequestType.Fortunes => Fortunes(Writer),
            RequestType.SingleQuery => SingleQuery(Writer),
            RequestType.Caching => Caching(Writer, _queries),
            RequestType.Updates => Updates(Writer, _queries),
            RequestType.MultipleQueries => MultipleQueries(Writer, _queries),
            _ => Default(Writer)
        };

        private static Task Default(PipeWriter pipeWriter)
        {
            var writer = GetWriter(pipeWriter, sizeHint: _defaultPreamble.Length + DateHeader.HeaderBytes.Length);
            Default(ref writer);
            writer.Commit();
            return Task.CompletedTask;
        }
#endif
        private readonly static AsciiString _defaultPreamble =
            _http11NotFound +
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
            Caching,
            Updates,
            MultipleQueries
        }
    }
}