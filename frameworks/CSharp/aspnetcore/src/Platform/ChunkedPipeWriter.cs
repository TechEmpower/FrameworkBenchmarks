// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;
using System.Buffers;
using System.Buffers.Text;
using System.Diagnostics;
using System.IO.Pipelines;
using System.Numerics;
using System.Runtime.CompilerServices;
using System.Threading;
using System.Threading.Tasks;

namespace PlatformBenchmarks;

internal sealed class ChunkedPipeWriter : PipeWriter
{
    private const int DefaultChunkSizeHint = 2048;
    private static readonly StandardFormat DefaultHexFormat = GetHexFormat(DefaultChunkSizeHint);
    private static ReadOnlySpan<byte> ChunkTerminator => "\r\n"u8;

    private PipeWriter _output;
    private int _chunkSizeHint;
    private StandardFormat _hexFormat = DefaultHexFormat;
    private Memory<byte> _currentFullChunk;
    private Memory<byte> _currentChunk;
    private int _buffered;
    private long _unflushedBytes;
    private bool _ended = false;

    public Memory<byte> Memory => _currentChunk;

    public PipeWriter Output => _output;

    public int Buffered => _buffered;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void SetOutput(PipeWriter output, int chunkSizeHint = DefaultChunkSizeHint)
    {
        _buffered = 0;
        _unflushedBytes = 0;
        _chunkSizeHint = chunkSizeHint;
        _output = output;

        StartNewChunk(chunkSizeHint, isFirst: true);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Reset()
    {
        _buffered = 0;
        _unflushedBytes = 0;
        _output = default;
        _ended = false;
        _hexFormat = DefaultHexFormat;
        _currentFullChunk = default;
        _currentChunk = default;
    }

    public override bool CanGetUnflushedBytes => true;

    public override long UnflushedBytes => _unflushedBytes;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public override void Advance(int count)
    {
        ThrowIfEnded();

        _buffered += count;
        _unflushedBytes += count;
        _currentChunk = _currentChunk[count..];
    }

    public override Memory<byte> GetMemory(int sizeHint = 0)
    {
        ThrowIfEnded();

        if (_currentChunk.Length <= sizeHint)
        {
            EnsureMore(sizeHint);
        }
        return _currentChunk;
    }

    public override Span<byte> GetSpan(int sizeHint = 0) => GetMemory(sizeHint).Span;

    public override void CancelPendingFlush()
    {
        _output.CancelPendingFlush();
    }

    public override void Complete(Exception exception = null)
    {
        ThrowIfEnded();

        CommitCurrentChunk(isFinal: true);

        _ended = true;
    }

    public override ValueTask<FlushResult> FlushAsync(CancellationToken cancellationToken = default)
    {
        CommitCurrentChunk(isFinal: false);

        var flushTask = _output.FlushAsync(cancellationToken);

        _unflushedBytes = 0;

        return flushTask;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static StandardFormat GetHexFormat(int maxValue)
    {
        var hexDigitCount = CountHexDigits(maxValue);

        return new StandardFormat('X', (byte)hexDigitCount);
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static int CountHexDigits(int n) => n <= 16 ? 1 : (BitOperations.Log2((uint)n) >> 2) + 1;

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void StartNewChunk(int sizeHint, bool isFirst = false)
    {
        ThrowIfEnded();

        // Header is like:
        // 520\r\n

        var oldFullChunkHexLength = -1;
        if (!isFirst)
        {
            oldFullChunkHexLength = CountHexDigits(_currentFullChunk.Length);
        }
        _currentFullChunk = _output.GetMemory(Math.Max(_chunkSizeHint, sizeHint));
        var newFullChunkHexLength = CountHexDigits(_currentFullChunk.Length);

        var currentFullChunkSpan = _currentFullChunk.Span;

        // Write space for HEX digits
        currentFullChunkSpan[..newFullChunkHexLength].Fill(48); // 48 == '0'

        // Write header terminator
        var terminator = "\r\n"u8;
        terminator.CopyTo(currentFullChunkSpan[newFullChunkHexLength..]);
        var chunkHeaderLength = newFullChunkHexLength + terminator.Length;
        _currentChunk = _currentFullChunk[chunkHeaderLength..];

        if ((!isFirst && oldFullChunkHexLength != newFullChunkHexLength) || (isFirst && DefaultChunkSizeHint != _chunkSizeHint))
        {
            // Update HEX format if changed
            _hexFormat = GetHexFormat(_currentFullChunk.Length);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private void CommitCurrentChunk(bool isFinal = false, int sizeHint = 0)
    {
        ThrowIfEnded();

        var contentLength = _buffered;
        
        if (contentLength > 0)
        {
            // Update the chunk header
            var chunkLengthHexDigitsLength = CountHexDigits(contentLength);
            var span = _currentFullChunk.Span;
            if (!Utf8Formatter.TryFormat(contentLength, span, out var bytesWritten, _hexFormat))
            {
                throw new NotSupportedException("Chunk size too large");
            }
            Debug.Assert(chunkLengthHexDigitsLength == bytesWritten, "HEX formatting math problem.");
            var headerLength = chunkLengthHexDigitsLength + 2;

            // Total chunk length: content length as HEX string + \r\n + content + \r\n
            var spanOffset = headerLength + contentLength;
            var chunkTotalLength = spanOffset + ChunkTerminator.Length;

            Debug.Assert(span.Length >= chunkTotalLength, "Bad chunk size calculation.");

            // Write out the chunk terminator
            ChunkTerminator.CopyTo(span[spanOffset..]);
            spanOffset = chunkTotalLength;
            
            if (!isFinal)
            {
                _output.Advance(chunkTotalLength);
                StartNewChunk(sizeHint);
            }
            else
            {
                // Write out final chunk (zero-length chunk)
                var terminator = "0\r\n\r\n"u8;
                if ((spanOffset + terminator.Length) <= span.Length)
                {
                    // There's space for the final chunk in the current span
                    terminator.CopyTo(span[spanOffset..]);
                    _output.Advance(chunkTotalLength + terminator.Length);
                }
                else
                {
                    // Final chunk doesn't fit in current span so just write it directly after advancing the writer
                    _output.Advance(chunkTotalLength);
                    _output.Write(terminator);
                }
            }
            
            _buffered = 0;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public void Write(ReadOnlySpan<byte> source)
    {
        ThrowIfEnded();

        if (_currentChunk.Length >= (source.Length + ChunkTerminator.Length))
        {
            source.CopyTo(_currentChunk.Span);
            Advance(source.Length);
        }
        else
        {
            WriteMultiBuffer(source);
        }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    private void EnsureMore(int count = 0)
    {
        if (count > (_currentChunk.Length - _buffered - ChunkTerminator.Length))
        {
            if (_buffered > 0)
            {
                CommitCurrentChunk(isFinal: false, count);
            }
            else
            {
                StartNewChunk(count);
            }
        }
    }

    private void WriteMultiBuffer(ReadOnlySpan<byte> source)
    {
        while (source.Length > 0)
        {
            if ((_currentChunk.Length - ChunkTerminator.Length) == 0)
            {
                EnsureMore();
            }

            var writable = Math.Min(source.Length, _currentChunk.Length - ChunkTerminator.Length);
            source[..writable].CopyTo(_currentChunk.Span);
            source = source[writable..];
            Advance(writable);
        }
    }

    private void ThrowIfEnded()
    {
        if (_ended)
        {
            throw new InvalidOperationException("Cannot use the writer after calling End().");
        }
    }
}