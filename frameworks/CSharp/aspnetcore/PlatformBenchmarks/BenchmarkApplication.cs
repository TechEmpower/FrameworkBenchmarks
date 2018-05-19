﻿// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Collections.Generic;
using System.IO.Pipelines;
using System.Text.Encodings.Web;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Server.Kestrel.Core.Internal.Http;
using Utf8Json;

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

        public static IDb Db { get; set; }

        public static class Paths
        {
            public readonly static AsciiString Plaintext = "/plaintext";
            public readonly static AsciiString Json = "/json";
            public readonly static AsciiString Fortunes = "/fortunes";
        }

        private RequestType _requestType;

        public void OnStartLine(HttpMethod method, HttpVersion version, Span<byte> target, Span<byte> path, Span<byte> query, Span<byte> customMethod, bool pathEncoded)
        {
            var requestType = RequestType.NotRecognized;
            if (method == HttpMethod.Get)
            {
                if (Paths.Plaintext.Length <= path.Length && path.StartsWith(Paths.Plaintext))
                {
                    requestType = RequestType.PlainText;
                }
                else if (Paths.Json.Length <= path.Length && path.StartsWith(Paths.Json))
                {
                    requestType = RequestType.Json;
                }
                else if (Paths.Fortunes.Length <= path.Length && path.StartsWith(Paths.Fortunes))
                {
                    requestType = RequestType.Fortunes;
                }
            }

            _requestType = requestType;
        }

        public Task ProcessRequestAsync()
        {
            if (_requestType == RequestType.PlainText)
            {
                PlainText(Writer);
            }
            else if (_requestType == RequestType.Json)
            {
                Json(Writer);
            }
            else if (_requestType == RequestType.Fortunes)
            {
                return Fortunes(Writer);
            }
            else
            {
                Default(Writer);
            }

            return Task.CompletedTask;
        }

        private static void PlainText(PipeWriter pipeWriter)
        {
            var writer = GetWriter(pipeWriter);

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
            writer.Commit();
        }

        private static void Json(PipeWriter pipeWriter)
        {
            var writer = GetWriter(pipeWriter);

            // HTTP 1.1 OK
            writer.Write(_http11OK);

            // Server headers
            writer.Write(_headerServer);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            // Content-Type header
            writer.Write(_headerContentTypeJson);

            // Content-Length header
            writer.Write(_headerContentLength);
            var jsonPayload = JsonSerializer.SerializeUnsafe(new { message = "Hello, World!" });
            writer.WriteNumeric((uint)jsonPayload.Count);

            // End of headers
            writer.Write(_eoh);

            // Body
            writer.Write(jsonPayload);
            writer.Commit();
        }

        private async Task Fortunes(PipeWriter pipeWriter)
        {
            OutputFortunes(pipeWriter, await Db.LoadFortunesRows());
        }

        private void OutputFortunes(PipeWriter pipeWriter, List<Fortune> model)
        {
            var writer = GetWriter(pipeWriter);

            // HTTP 1.1 OK
            writer.Write(_http11OK);

            // Server headers
            writer.Write(_headerServer);

            // Date header
            writer.Write(DateHeader.HeaderBytes);

            // Content-Type header
            writer.Write(_headerContentTypeHtml);

            // Content-Length header
            writer.Write(_headerContentLength);

            var lengthWriter = writer;
            writer.Write(_contentLengthGap);

            // End of headers
            writer.Write(_eoh);

            var bodyStart = writer.Buffered;
            // Body
            writer.Write(_fortunesTableStart);
            foreach (var item in model)
            {
                writer.Write(_fortunesRowStart);
                writer.WriteNumeric((uint)item.Id);
                writer.Write(_fortunesColumn);
                writer.WriteUtf8String(HtmlEncoder.Encode(item.Message));
                writer.Write(_fortunesRowEnd);
            }
            writer.Write(_fortunesTableEnd);
            lengthWriter.WriteNumeric((uint)(writer.Buffered - bodyStart));

            writer.Commit();
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
            Fortunes
        }
    }
}
