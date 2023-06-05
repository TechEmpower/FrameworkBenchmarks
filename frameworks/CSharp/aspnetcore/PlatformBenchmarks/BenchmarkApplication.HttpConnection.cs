// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Buffers;
using System.IO.Pipelines;
using System.Runtime.CompilerServices;
using System.Text.Encodings.Web;
using System.Text.Unicode;
using Microsoft.AspNetCore.Server.Kestrel.Core.Internal.Http;

namespace PlatformBenchmarks;

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
        var writer = GetWriter(Writer, sizeHint: 160 * 16); // 160*16 is for Plaintext, for Json 160 would be enough

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
            if (Parser.ParseRequestLine(new ParsingAdapter(this), ref reader))
            {
                state = State.Headers;
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
                var isCompleted = readResult.IsCompleted;

                if (buffer.IsEmpty && isCompleted)
                {
                    return;
                }

                while (true)
                {
                    if (!ParseHttpRequest(ref buffer, isCompleted))
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
                    Reader.AdvanceTo(buffer.Start, buffer.End);
                    break;
                }

                await Writer.FlushAsync();
            }
        }

        private bool ParseHttpRequest(ref ReadOnlySequence<byte> buffer, bool isCompleted)
        {
            var reader = new SequenceReader<byte>(buffer);
            var state = _state;

            if (state == State.StartLine)
            {
                if (Parser.ParseRequestLine(new ParsingAdapter(this), ref reader))
                {
                    state = State.Headers;
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

            if (state == State.Body)
            {
                // Complete request read, consumed and examined are the same (length 0)
                buffer = buffer.Slice(reader.Position, 0);
            }
            else
            {
                // In-complete request read, consumed is current position and examined is the remaining.
                buffer = buffer.Slice(reader.Position);
            }

            return true;
        }
#endif

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
    private static BufferWriter<WriterAdapter> GetWriter(PipeWriter pipeWriter, int sizeHint)
        => new(new(pipeWriter), sizeHint);

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static ChunkedBufferWriter<WriterAdapter> GetChunkedWriter(PipeWriter pipeWriter, int chunkSizeHint)
    {
        var writer = ChunkedWriterPool.Get();
        writer.SetOutput(new WriterAdapter(pipeWriter), chunkSizeHint);
        return writer;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void ReturnChunkedWriter(ChunkedBufferWriter<WriterAdapter> writer) => ChunkedWriterPool.Return(writer);

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

        public void OnStaticIndexedHeader(int index)
            => RequestHandler.OnStaticIndexedHeader(index);

        public void OnStaticIndexedHeader(int index, ReadOnlySpan<byte> value)
            => RequestHandler.OnStaticIndexedHeader(index, value);

        public void OnHeader(ReadOnlySpan<byte> name, ReadOnlySpan<byte> value)
            => RequestHandler.OnHeader(name, value);

        public void OnHeadersComplete(bool endStream)
            => RequestHandler.OnHeadersComplete(endStream);

        public void OnStartLine(HttpVersionAndMethod versionAndMethod, TargetOffsetPathLength targetPath, Span<byte> startLine)
            => RequestHandler.OnStartLine(versionAndMethod, targetPath, startLine);
    }
}
