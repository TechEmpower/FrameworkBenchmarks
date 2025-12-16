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
using TouchSocket.Core;
using TouchSocket.Sockets;

namespace TouchSocketHttpPlatform;

internal class Program
{
    private static async Task Main(string[] args)
    {
        MyServer server = new MyServer();
        await server.SetupAsync(new TouchSocketConfig()
            .SetListenIPHosts(8080)
            .SetMaxCount(1000000)
            .SetTransportOption(options =>
            {
                options.BufferOnDemand = false;

                options.ReceivePipeOptions = new PipeOptions(
              pool: MemoryPool<byte>.Shared,
              readerScheduler: PipeScheduler.Inline,
              writerScheduler: PipeScheduler.Inline,
              pauseWriterThreshold: 1024 * 1024,
              resumeWriterThreshold: 1024 * 512,
              minimumSegmentSize: -1,
              useSynchronizationContext: false);

                options.SendPipeOptions = new PipeOptions(
              pool: MemoryPool<byte>.Shared,
              readerScheduler: PipeScheduler.Inline,
              writerScheduler: PipeScheduler.Inline,
              pauseWriterThreshold: 64 * 1024,
              resumeWriterThreshold: 32 * 1024,
              minimumSegmentSize: -1,
              useSynchronizationContext: false);
            })
            .ConfigureContainer(a =>
            {
                a.AddConsoleLogger();
            }));

        await server.StartAsync();
        Console.WriteLine("HTTP服务器已启动，端口: 8080");

        while (true)
        {
            Console.ReadLine();
        }
    }
}
