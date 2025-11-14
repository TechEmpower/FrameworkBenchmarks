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

namespace HttpServerLinePerformanceConsoleApp;

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
    public static ReadOnlySpan<byte> Json => "/json"u8;
    public static ReadOnlySpan<byte> Plaintext => "/plaintext"u8;
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
            var readResult = await pipeReader.ReadAsync();
            var bufferSequence = readResult.Buffer;

            var totalConsumed = ProcessRequests(bufferSequence, pipeWriter, out var responseCount);

            if (responseCount > 0)
            {
                await pipeWriter.FlushAsync();
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

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static long ProcessRequests(ReadOnlySequence<byte> buffer, PipeWriter writer, out int responseCount)
    {
        var seqReader = new SequenceReader<byte>(buffer);
        var totalConsumed = 0L;
        responseCount = 0;

        while (!seqReader.End)
        {
            var startConsumed = seqReader.Consumed;

            // 请求行
            if (!TryReadLine(ref seqReader, out var requestLineLength))
            {
                // 请求行不完整，不消费任何数据
                break;
            }

            var requestLineConsumed = requestLineLength + 2;

            // 读取Headers，直到空行；若不完整，回退并等待更多数据
            var headersStartConsumed = seqReader.Consumed;
            bool headersComplete = false;
            while (!seqReader.End)
            {
                if (!TryReadLine(ref seqReader, out var headerLength))
                {
                    // 回退到读取Headers前的位置，等待更多数据
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
                    break; // headers 结束
                }
            }

            if (!headersComplete)
            {
                // 不完整，等待更多数据
                break;
            }

            // 解析URL - 直接在原始位置解析，避免Slice
            var routeType = ParseUrlFast(buffer.Slice(startConsumed), requestLineConsumed);

            // 计算本次消费的字节数
            var consumed = seqReader.Consumed - startConsumed;
            totalConsumed += consumed;

            // 写入响应
            WriteResponseSync(writer, routeType);
            responseCount++;
        }

        return totalConsumed;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void WriteResponseSync(PipeWriter writer, RouteType routeType)
    {
        switch (routeType)
        {
            case RouteType.Plaintext:
                writer.Write(PlaintextPreamble);
                writer.Write(DateHeader.HeaderBytes);
                writer.Write(PlainTextBody);
                break;
            case RouteType.Json:
                writer.Write(JsonPreamble);
                writer.Write(DateHeader.HeaderBytes);
                writer.Write(JsonBody);
                break;
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static bool TryReadLine(ref SequenceReader<byte> reader, out long length)
    {
        var start = reader.Consumed;

        while (!reader.End)
        {
            if (reader.TryRead(out var b))
            {
                if (b == '\r')
                {
                    // 查看是否有'\n'，若暂不可用则回退并判定不完整
                    if (!reader.TryPeek(out var next))
                    {
                        // 回退已读取的'\r'
                        reader.Rewind(1);
                        length = 0;
                        return false;
                    }
                    if (next == '\n')
                    {
                        // 消费'\n'并返回本行长度（不含CRLF）
                        reader.Advance(1);
                        length = reader.Consumed - start - 2;
                        return true;
                    }
                }
            }
        }

        length = 0;
        return false;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static RouteType ParseUrlFast(ReadOnlySequence<byte> sequence, long requestLineLength)
    {
        var reader = new SequenceReader<byte>(sequence);

        // 跳过方法
        var spaceCount = 0;
        var urlStart = 0L;
        var urlEnd = 0L;

        while (reader.Consumed < requestLineLength && !reader.End)
        {
            if (reader.TryRead(out var b))
            {
                if (b == ' ')
                {
                    spaceCount++;
                    if (spaceCount == 1)
                    {
                        urlStart = reader.Consumed;
                    }
                    else if (spaceCount == 2)
                    {
                        urlEnd = reader.Consumed - 1;
                        break;
                    }
                }
            }
        }

        if (spaceCount < 2)
        {
            return RouteType.Unknown;
        }

        var urlLength = urlEnd - urlStart;

        // 截取URL片段
        var startPos = sequence.GetPosition(urlStart);
        var urlSlice = sequence.Slice(startPos, urlLength);

        // "/plaintext"
        if (urlLength == Plaintext.Length)
        {
            if (urlSlice.IsSingleSegment)
            {
                if (urlSlice.FirstSpan.SequenceEqual(Plaintext))
                {
                    return RouteType.Plaintext;
                }
            }
            else
            {
                Span<byte> tmp = stackalloc byte[(int)urlLength];
                urlSlice.CopyTo(tmp);
                if (tmp.SequenceEqual(Plaintext))
                {
                    return RouteType.Plaintext;
                }
            }
        }
        // "/json"
        else if (urlLength == Json.Length)
        {
            if (urlSlice.IsSingleSegment)
            {
                if (urlSlice.FirstSpan.SequenceEqual(Json))
                {
                    return RouteType.Json;
                }
            }
            else
            {
                Span<byte> tmp = stackalloc byte[(int)urlLength];
                urlSlice.CopyTo(tmp);
                if (tmp.SequenceEqual(Json))
                {
                    return RouteType.Json;
                }
            }
        }

        return RouteType.Unknown;
    }
}

