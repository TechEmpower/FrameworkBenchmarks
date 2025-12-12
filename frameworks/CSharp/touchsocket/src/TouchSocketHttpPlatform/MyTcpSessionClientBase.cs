// ------------------------------------------------------------------------------
// 此代码版权（除特别声明或在XREF结尾的命名空间的代码）归作者本人若汝棋茗所有
// 源代码使用协议遵循本仓库的开源协议及附加协议，若本仓库没有设置，则按MIT开源协议授权
// CSDN博客：https://blog.csdn.net/qq_40374647
// 哔哩哔哩视频：https://space.bilibili.com/94253567
// Gitee源代码仓库：https://gitee.com/RRQM_Home
// Github源代码仓库：https://github.com/RRQM
// API首页：https://touchsocket.net/
// 交流QQ群：234762506
// 感谢您的下载和使用
// ------------------------------------------------------------------------------

using System.Buffers;
using System.IO.Pipelines;
using System.Runtime.CompilerServices;
using TouchSocket.Sockets;

namespace TouchSocketHttpPlatform;

/// <summary>
/// 路由类型
/// </summary>
internal enum RouteType
{
    /// <summary>
    /// 未知路由
    /// </summary>
    Unknown,

    /// <summary>
    /// 纯文本路由
    /// </summary>
    Plaintext,

    /// <summary>
    /// JSON路由
    /// </summary>
    Json
}

internal sealed class MyTcpSessionClientBase : TcpSessionClient
{
    #region Paths
    private static ReadOnlySpan<byte> Json => "/json"u8;
    private static ReadOnlySpan<byte> Plaintext => "/plaintext"u8;
    #endregion

    private static ReadOnlySpan<byte> PlainTextBody => "Hello, World!"u8;
    private static ReadOnlySpan<byte> JsonBody => "{\"message\":\"Hello, World!\"}"u8;

    private static ReadOnlySpan<byte> PlaintextPreamble =>
           "HTTP/1.1 200 OK\r\n"u8 +
           "Server: T\r\n"u8 +
           "Content-Type: text/plain\r\n"u8 +
           "Content-Length: 13\r\n"u8;

    private static ReadOnlySpan<byte> JsonPreamble =>
        "HTTP/1.1 200 OK\r\n"u8 +
        "Server: T\r\n"u8 +
        "Content-Type: application/json\r\n"u8 +
        "Content-Length: 27\r\n"u8;

    protected override async Task ReceiveLoopAsync(ITransport transport)
    {
        var pipeReader = transport.Reader;
        var pipeWriter = transport.Writer;

        while (true)
        {
            ValueTask<ReadResult> readTask = pipeReader.ReadAsync();
            ReadResult readResult;

            if (readTask.IsCompleted)
            {
                readResult = readTask.Result;
            }
            else
            {
                readResult = await readTask.ConfigureAwait(false);
            }

            var bufferSequence = readResult.Buffer;

            var totalConsumed = ProcessRequests(bufferSequence, pipeWriter, out var responseCount);

            if (responseCount > 0)
            {
                ValueTask<FlushResult> flushTask = pipeWriter.FlushAsync();
                
                if (!flushTask.IsCompleted)
                {
                    await flushTask.ConfigureAwait(false);
                }
            }

            if (totalConsumed > 0)
            {
                pipeReader.AdvanceTo(bufferSequence.GetPosition(totalConsumed));
            }
            else
            {
                pipeReader.AdvanceTo(bufferSequence.Start, bufferSequence.End);
            }

            if (readResult.IsCompleted)
            {
                break;
            }
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
    private static long ProcessRequests(ReadOnlySequence<byte> buffer, PipeWriter writer, out int responseCount)
    {
        var seqReader = new SequenceReader<byte>(buffer);
        var totalConsumed = 0L;
        responseCount = 0;

        while (!seqReader.End)
        {
            var startConsumed = seqReader.Consumed;

            if (!TryReadLine(ref seqReader, out var requestLineLength))
            {
                break;
            }

            var requestLineConsumed = requestLineLength + 2;

            var headersStartConsumed = seqReader.Consumed;
            bool headersComplete = false;
            
            while (!seqReader.End)
            {
                if (!TryReadLine(ref seqReader, out var headerLength))
                {
                    var rewind = seqReader.Consumed - headersStartConsumed;
                    if (rewind > 0)
                    {
                        seqReader.Rewind(rewind);
                    }
                    headersComplete = false;
                    break;
                }

                if (headerLength == 0)
                {
                    headersComplete = true;
                    break;
                }
            }

            if (!headersComplete)
            {
                break;
            }

            var routeType = ParseUrlFast(buffer.Slice(startConsumed), requestLineConsumed);

            var consumed = seqReader.Consumed - startConsumed;
            totalConsumed += consumed;

            WriteResponseSync(writer, routeType);
            responseCount++;
        }

        return totalConsumed;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
    private static void WriteResponseSync(PipeWriter writer, RouteType routeType)
    {
        if (routeType == RouteType.Plaintext)
        {
            writer.Write(PlaintextPreamble);
            writer.Write(DateHeader.HeaderBytes);
            writer.Write(PlainTextBody);
        }
        else if (routeType == RouteType.Json)
        {
            writer.Write(JsonPreamble);
            writer.Write(DateHeader.HeaderBytes);
            writer.Write(JsonBody);
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
    private static bool TryReadLine(ref SequenceReader<byte> reader, out long length)
    {
        if (reader.TryAdvanceTo((byte)'\r', advancePastDelimiter: false))
        {
            var start = reader.Consumed;
            
            if (!reader.TryPeek(1, out var next))
            {
                length = 0;
                return false;
            }
            
            if (next == '\n')
            {
                var lineLength = reader.Consumed;
                reader.Advance(2);
                length = lineLength - start;
                return true;
            }
            
            reader.Advance(1);
            return TryReadLine(ref reader, out length);
        }

        length = 0;
        return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining | MethodImplOptions.AggressiveOptimization)]
    private static RouteType ParseUrlFast(ReadOnlySequence<byte> sequence, long requestLineLength)
    {
        var reader = new SequenceReader<byte>(sequence);

        if (!reader.TryAdvanceTo((byte)' ', advancePastDelimiter: true))
        {
            return RouteType.Unknown;
        }

        var urlStart = reader.Consumed;

        if (!reader.TryAdvanceTo((byte)' ', advancePastDelimiter: false))
        {
            return RouteType.Unknown;
        }

        var urlLength = reader.Consumed - urlStart;

        if (urlLength == Plaintext.Length)
        {
            var urlSlice = sequence.Slice(urlStart, urlLength);
            
            if (urlSlice.IsSingleSegment)
            {
                return urlSlice.FirstSpan.SequenceEqual(Plaintext) ? RouteType.Plaintext : RouteType.Unknown;
            }
            
            Span<byte> tmp = stackalloc byte[(int)urlLength];
            urlSlice.CopyTo(tmp);
            return tmp.SequenceEqual(Plaintext) ? RouteType.Plaintext : RouteType.Unknown;
        }
        
        if (urlLength == Json.Length)
        {
            var urlSlice = sequence.Slice(urlStart, urlLength);
            
            if (urlSlice.IsSingleSegment)
            {
                return urlSlice.FirstSpan.SequenceEqual(Json) ? RouteType.Json : RouteType.Unknown;
            }
            
            Span<byte> tmp = stackalloc byte[(int)urlLength];
            urlSlice.CopyTo(tmp);
            return tmp.SequenceEqual(Json) ? RouteType.Json : RouteType.Unknown;
        }

        return RouteType.Unknown;
    }
}

