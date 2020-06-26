// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System;
using System.Buffers;
using System.IO.Pipelines;
using System.Runtime.CompilerServices;
using System.Text.Encodings.Web;
using System.Text.Unicode;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Server.Kestrel.Core.Internal.Http;

namespace PlatformBenchmarks
{
    public partial class BenchmarkApplication : IHttpConnection
    {
        private State _state;

        public PipeReader Reader { get; set; }
        public PipeWriter Writer { get; set; }

        protected HtmlEncoder HtmlEncoder { get; } = CreateHtmlEncoder();

        private HttpParser<ParsingAdapter> Parser { get; } = new HttpParser<ParsingAdapter>();

        public async Task ExecuteAsync()
        {
            try
            {
                await ProcessRequestsAsync();

                Reader.Complete();
            }
            catch (Exception ex)
            {
                Reader.Complete(ex);
            }
            finally
            {
                Writer.Complete();
            }
        }

        private static HtmlEncoder CreateHtmlEncoder()
        {
            var settings = new TextEncoderSettings(UnicodeRanges.BasicLatin, UnicodeRanges.Katakana, UnicodeRanges.Hiragana);
            settings.AllowCharacter('\u2014');  // allow EM DASH through
            return HtmlEncoder.Create(settings);
        }

#if !DATABASE
        private async Task ProcessRequestsAsync()
        {
            while (true)
            {
                var readResult = await Reader.ReadAsync(default);
                var buffer = readResult.Buffer;
                var isCompleted = readResult.IsCompleted;

                if (buffer.IsEmpty && isCompleted)
                {
                    return;
                }

                if (!HandleRequests(buffer, isCompleted))
                {
                    return;
                }

                await Writer.FlushAsync(default);
            }
        }

        private bool HandleRequests(in ReadOnlySequence<byte> buffer, bool isCompleted)
        {
            var reader = new SequenceReader<byte>(buffer);
            var writer = GetWriter(Writer);

            while (true)
            {
                if (!ParseHttpRequest(ref reader, isCompleted))
                {
                    return false;
                }

                if (_state == State.Body)
                {
                    ProcessRequest(ref writer);

                    _state = State.StartLine;

                    if (!reader.End)
                    {
                        // More input data to parse
                        continue;
                    }
                }

                // No more input or incomplete data, Advance the Reader
                Reader.AdvanceTo(reader.Position, buffer.End);
                break;
            }

            writer.Commit();
            return true;
        }

        private bool ParseHttpRequest(ref SequenceReader<byte> reader, bool isCompleted)
        {
            var state = _state;

            if (state == State.StartLine)
            {
#if NETCOREAPP5_0
                var unconsumedSequence = reader.UnreadSequence;
#else
                var unconsumedSequence = reader.Sequence.Slice(reader.Position);
#endif
                if (Parser.ParseRequestLine(new ParsingAdapter(this), unconsumedSequence, out var consumed, out _))
                {
                    state = State.Headers;

                    var parsedLength = unconsumedSequence.Slice(reader.Position, consumed).Length;
                    reader.Advance(parsedLength);
                }
            }

            if (state == State.Headers)
            {
                var success = Parser.ParseHeaders(new ParsingAdapter(this), ref reader);

                if (success)
                {
                    state = State.Body;
                }
            }

            if (state != State.Body && isCompleted)
            {
                ThrowUnexpectedEndOfData();
            }

            _state = state;
            return true;
        }
#else
        private async Task ProcessRequestsAsync()
        {
            while (true)
            {
                var readResult = await Reader.ReadAsync();

                var buffer = readResult.Buffer;
                while (true)
                {
                    if (!ParseHttpRequest(ref buffer, readResult.IsCompleted, out var examined))
                    {
                        return;
                    }

                    if (_state == State.Body)
                    {
                        await ProcessRequestAsync();

                        _state = State.StartLine;

                        if (!buffer.IsEmpty)
                        {
                            // More input data to parse
                            continue;
                        }
                    }

                    // No more input or incomplete data, Advance the Reader
                    Reader.AdvanceTo(buffer.Start, examined);
                    break;
                }

                await Writer.FlushAsync();
            }
        }

        private bool ParseHttpRequest(ref ReadOnlySequence<byte> buffer, bool isCompleted, out SequencePosition examined)
        {
            examined = buffer.End;
            var state = _state;

            if (!buffer.IsEmpty)
            {
                SequencePosition consumed;
                if (state == State.StartLine)
                {
                    if (Parser.ParseRequestLine(new ParsingAdapter(this), buffer, out consumed, out examined))
                    {
                        state = State.Headers;
                    }

                    buffer = buffer.Slice(consumed);
                }

                if (state == State.Headers)
                {
                    var reader = new SequenceReader<byte>(buffer);
                    var success = Parser.ParseHeaders(new ParsingAdapter(this), ref reader);

                    consumed = reader.Position;
                    if (success)
                    {
                        examined = consumed;
                        state = State.Body;
                    }
                    else
                    {
                        examined = buffer.End;
                    }

                    buffer = buffer.Slice(consumed);
                }

                if (state != State.Body && isCompleted)
                {
                    ThrowUnexpectedEndOfData();
                }
            }
            else if (isCompleted)
            {
                return false;
            }

            _state = state;
            return true;
        }
#endif

#if NETCOREAPP5_0

        public void OnStaticIndexedHeader(int index)
        {
        }

        public void OnStaticIndexedHeader(int index, ReadOnlySpan<byte> value)
        {
        }

        public void OnHeader(ReadOnlySpan<byte> name, ReadOnlySpan<byte> value)
        {
        }
        public void OnHeadersComplete(bool endStream)
        {
        }
#else
        public void OnHeader(Span<byte> name, Span<byte> value)
        {
        }
        public void OnHeadersComplete()
        {
        }
#endif

        private static void ThrowUnexpectedEndOfData()
        {
            throw new InvalidOperationException("Unexpected end of data!");
        }

        private enum State
        {
            StartLine,
            Headers,
            Body
        }

        [MethodImpl(MethodImplOptions.AggressiveInlining)]
        private static BufferWriter<WriterAdapter> GetWriter(PipeWriter pipeWriter)
            => new BufferWriter<WriterAdapter>(new WriterAdapter(pipeWriter));

        private struct WriterAdapter : IBufferWriter<byte>
        {
            public PipeWriter Writer;

            public WriterAdapter(PipeWriter writer)
                => Writer = writer;

            public void Advance(int count)
                => Writer.Advance(count);

            public Memory<byte> GetMemory(int sizeHint = 0)
                => Writer.GetMemory(sizeHint);

            public Span<byte> GetSpan(int sizeHint = 0)
                => Writer.GetSpan(sizeHint);
        }

        private struct ParsingAdapter : IHttpRequestLineHandler, IHttpHeadersHandler
        {
            public BenchmarkApplication RequestHandler;

            public ParsingAdapter(BenchmarkApplication requestHandler)
                => RequestHandler = requestHandler;

#if NETCOREAPP5_0
            public void OnStaticIndexedHeader(int index) 
                => RequestHandler.OnStaticIndexedHeader(index);

            public void OnStaticIndexedHeader(int index, ReadOnlySpan<byte> value)
                => RequestHandler.OnStaticIndexedHeader(index, value);

            public void OnHeader(ReadOnlySpan<byte> name, ReadOnlySpan<byte> value)
                => RequestHandler.OnHeader(name, value);

            public void OnHeadersComplete(bool endStream)
                => RequestHandler.OnHeadersComplete(endStream);
#else
            public void OnHeader(Span<byte> name, Span<byte> value)
                => RequestHandler.OnHeader(name, value);
            public void OnHeadersComplete()
                => RequestHandler.OnHeadersComplete();
#endif

            public void OnStartLine(HttpMethod method, HttpVersion version, Span<byte> target, Span<byte> path, Span<byte> query, Span<byte> customMethod, bool pathEncoded)
                => RequestHandler.OnStartLine(method, version, target, path, query, customMethod, pathEncoded);
        }
    }
}
