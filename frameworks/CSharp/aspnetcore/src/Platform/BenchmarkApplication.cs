// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Buffers.Text;
using System.Collections.Generic;
using System.IO.Pipelines;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Server.Kestrel.Core.Internal.Http;
using Microsoft.Extensions.ObjectPool;
#if !AOT
using RazorSlices;
#endif

namespace PlatformBenchmarks
{
    public sealed partial class BenchmarkApplication
    {
        public static ReadOnlySpan<byte> ApplicationName => "Kestrel Platform-Level Application"u8;

        private static ReadOnlySpan<byte> _crlf => "\r\n"u8;
        private static ReadOnlySpan<byte> _eoh => "\r\n\r\n"u8; // End Of Headers
        private static ReadOnlySpan<byte> _http11OK => "HTTP/1.1 200 OK\r\n"u8;
        private static ReadOnlySpan<byte> _http11NotFound => "HTTP/1.1 404 Not Found\r\n"u8;
        private static ReadOnlySpan<byte> _headerServer => "Server: K"u8;
        private static ReadOnlySpan<byte> _headerContentLength => "Content-Length: "u8;
        private static ReadOnlySpan<byte> _headerContentLengthZero => "Content-Length: 0"u8;
        private static ReadOnlySpan<byte> _headerContentTypeText => "Content-Type: text/plain"u8;
        private static ReadOnlySpan<byte> _headerContentTypeJson => "Content-Type: application/json"u8;
        private static ReadOnlySpan<byte> _headerContentTypeHtml => "Content-Type: text/html; charset=UTF-8"u8;

        private static ReadOnlySpan<byte> _dbPreamble => 
            "HTTP/1.1 200 OK\r\n"u8 +
            "Server: K\r\n"u8 +
            "Content-Type: application/json\r\n"u8 +
            "Content-Length: "u8;

        private static ReadOnlySpan<byte> _plainTextBody => "Hello, World!"u8;
        private static ReadOnlySpan<byte> _contentLengthGap => "    "u8;

        public static RawDb RawDb { get; set; }

        private static readonly DefaultObjectPool<ChunkedBufferWriter<WriterAdapter>> ChunkedWriterPool
            = new(new ChunkedWriterObjectPolicy());

        private sealed class ChunkedWriterObjectPolicy : IPooledObjectPolicy<ChunkedBufferWriter<WriterAdapter>>
        {
            public ChunkedBufferWriter<WriterAdapter> Create() => new();

            public bool Return(ChunkedBufferWriter<WriterAdapter> writer)
            {
                writer.Reset();
                return true;
            }
        }

#if !AOT
#if NPGSQL
        private readonly static SliceFactory<List<FortuneUtf8>> FortunesTemplateFactory = RazorSlice.ResolveSliceFactory<List<FortuneUtf8>>("/Templates/FortunesUtf8.cshtml");
#else
        private readonly static SliceFactory<List<FortuneUtf16>> FortunesTemplateFactory = RazorSlice.ResolveSliceFactory<List<FortuneUtf16>>("/Templates/FortunesUtf16.cshtml");
#endif
#endif

        [ThreadStatic]
        private static Utf8JsonWriter t_writer;

        private static readonly JsonContext SerializerContext = JsonContext.Default;

        [JsonSourceGenerationOptions(GenerationMode = JsonSourceGenerationMode.Serialization)]
        [JsonSerializable(typeof(JsonMessage))]
        [JsonSerializable(typeof(CachedWorld[]))]
        [JsonSerializable(typeof(World[]))]
        private partial class JsonContext : JsonSerializerContext
        {
        }

        public static class Paths
        {
            public static ReadOnlySpan<byte> Json => "/json"u8;
            public static ReadOnlySpan<byte> Plaintext => "/plaintext"u8;
            public static ReadOnlySpan<byte> SingleQuery => "/db"u8;
            public static ReadOnlySpan<byte> FortunesRaw => "/fortunes"u8;
            public static ReadOnlySpan<byte> Updates => "/updates/"u8;
            public static ReadOnlySpan<byte> MultipleQueries => "/queries/"u8;
            public static ReadOnlySpan<byte> Caching => "/cached-worlds/"u8;
        }

        private RequestType _requestType;
        private int _queries;

        public void OnStartLine(HttpVersionAndMethod versionAndMethod, TargetOffsetPathLength targetPath, Span<byte> startLine)
        {
            _requestType = versionAndMethod.Method == HttpMethod.Get ? GetRequestType(startLine.Slice(targetPath.Offset, targetPath.Length), ref _queries) : RequestType.NotRecognized;
        }

        private static RequestType GetRequestType(ReadOnlySpan<byte> path, ref int queries)
        {
            if (path.Length == 10 && path.SequenceEqual(Paths.Plaintext))
            {
                return RequestType.PlainText;
            }
            else if (path.Length == 5 && path.SequenceEqual(Paths.Json))
            {
                return RequestType.Json;
            }
            else if (path.Length == 3 && path[0] == '/' && path[1] == 'd' && path[2] == 'b')
            {
                return RequestType.SingleQuery;
            }
            else if (path.Length >= 9 && path[1] == 'f' && path.StartsWith(Paths.FortunesRaw))
            {
                return RequestType.FortunesRaw;
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

            return RequestType.NotRecognized;
        }

        private static int ParseQueries(ReadOnlySpan<byte> parameter)
        {
            if (!Utf8Parser.TryParse(parameter, out int queries, out _))
            {
                queries = 1;
            }
            else
            {
                queries = Math.Clamp(queries, 1, 500);
            }

            return queries;
        }

        private bool ProcessRequest(ref BufferWriter<WriterAdapter> writer)
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
                return false;
            }

            return true;
        }

        private Task ProcessRequestAsync() => _requestType switch
        {
#if !AOT
            RequestType.FortunesRaw => FortunesRaw(Writer),
#endif
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

        private static ReadOnlySpan<byte> _defaultPreamble =>
            "HTTP/1.1 200 OK\r\n"u8 +
            "Server: K\r\n"u8 +
            "Content-Type: text/plain\r\n"u8 +
            "Content-Length: 0"u8;

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
            FortunesRaw,
            SingleQuery,
            Caching,
            Updates,
            MultipleQueries
        }
    }
}
