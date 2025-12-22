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
// ----------------------------------------------------------------------------

using System.Buffers;
using System.Net;
using System.Net.Sockets;
using System.Runtime.CompilerServices;
using TouchSocket.Core;

namespace TouchSocketHttpSyncPlatform;

internal enum RouteType
{
    Unknown,
    Plaintext,
    Json
}

internal class Program
{
    private static Socket? _listenerSocket;
    private static bool _isRunning;

    private static ReadOnlySpan<byte> Json => "/json"u8;
    private static ReadOnlySpan<byte> Plaintext => "/plaintext"u8;
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

    static void Main(string[] args)
    {
        var port = 8080;
        _listenerSocket = new Socket(AddressFamily.InterNetwork, SocketType.Stream, ProtocolType.Tcp);
        _listenerSocket.Bind(new IPEndPoint(IPAddress.Any, port));
        _listenerSocket.Listen(1000);
        _isRunning = true;

        Console.WriteLine($"HTTP服务器已启动，监听端口: {port}");
        Console.WriteLine("等待客户端连接...");

        var acceptThread = new Thread(AcceptClients)
        {
            IsBackground = false
        };
        acceptThread.Start();

        DateHeader.SyncDateTimer();


        while (true)
        {
            Console.ReadLine();

            _isRunning = false;
            _listenerSocket.Close();
        }
    }

    private static void AcceptClients()
    {
        while (_isRunning)
        {
            try
            {
                var clientSocket = _listenerSocket!.Accept();
                
                var clientThread = new Thread(HandleClient)
                {
                    IsBackground = true
                };
                clientThread.Start(clientSocket);
            }
            catch (SocketException)
            {
                break;
            }
        }
    }

    private static void HandleClient(object? obj)
    {
        var clientSocket = (Socket)obj!;
        clientSocket.NoDelay = true;
        var remoteEndPoint = clientSocket.RemoteEndPoint?.ToString();
        
        Console.WriteLine($"客户端已连接: {remoteEndPoint}");

        try
        {
            var pipeReceive = new SegmentedPipe();
            var pipeSend = new SegmentedPipe();
            var reader = pipeReceive.Reader;
            var writer = pipeReceive.Writer;
           
            while (_isRunning && clientSocket.Connected)
            {
                var span = writer.GetSpan(1024 * 64);
                var bytesRead = clientSocket.Receive(span, SocketFlags.None);
                
                if (bytesRead == 0)
                {
                    break;
                }

                writer.Advance(bytesRead);

                ProcessData(reader,pipeSend.Writer);

                var sendResult = pipeSend.Reader.Read();
                var sendBuffer = sendResult.Buffer;
                if (sendBuffer.Length > 0)
                {
                    foreach (var segment in sendBuffer)
                    {
                        clientSocket.Send(segment.Span, SocketFlags.None);
                    }
                    pipeSend.Reader.AdvanceTo(sendBuffer.End);
                }
            }
        }
        catch (Exception ex)
        {
            Console.WriteLine($"处理客户端 {remoteEndPoint} 时发生错误: {ex.Message}");
        }
        finally
        {
            try
            {
                clientSocket.Shutdown(SocketShutdown.Both);
            }
            catch { }
            
            clientSocket.Close();
            Console.WriteLine($"客户端已断开: {remoteEndPoint}");
        }
    }

    private static void ProcessData(SegmentedPipeReader reader, SegmentedPipeWriter writer)
    {
        var result = reader.Read();
        var buffer = result.Buffer;

        var totalConsumed = ProcessRequests(buffer, writer, out var responseCount);

        if (totalConsumed > 0)
        {
            reader.AdvanceTo(buffer.GetPosition(totalConsumed));
        }
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static long ProcessRequests(ReadOnlySequence<byte> buffer, SegmentedPipeWriter writer, out int responseCount)
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

            WriteResponse(writer, routeType);
            responseCount++;
        }

        return totalConsumed;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void WriteResponse(SegmentedPipeWriter writer, RouteType routeType)
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
                    if (!reader.TryPeek(out var next))
                    {
                        reader.Rewind(1);
                        length = 0;
                        return false;
                    }
                    if (next == '\n')
                    {
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

        var startPos = sequence.GetPosition(urlStart);
        var urlSlice = sequence.Slice(startPos, urlLength);

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
