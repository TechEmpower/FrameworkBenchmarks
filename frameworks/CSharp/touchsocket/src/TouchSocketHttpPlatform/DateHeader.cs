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

using System.Buffers.Text;
using System.Diagnostics;
using TouchSocket.Core;
using TouchSocket.Sockets;

namespace HttpServerLinePerformanceConsoleApp;

internal static class DateHeader
{
    private const int dateTimeRLength = 29;
    private const int prefixLength = 6; // "Date: ".Length is 6 in bytes for ASCII
    private const int suffixIndex = dateTimeRLength + prefixLength;

    // Wed, 14 Mar 2018 14:20:00 GMT
    private const int suffixLength = 2; // crlf
    private static readonly Timer s_timer = new((s) =>
    {
        SetDateValues(DateTimeOffset.UtcNow);
    }, null, 1000, 1000);

    private static byte[] s_headerBytesMaster = new byte[prefixLength + dateTimeRLength + 2 * suffixLength];
    private static byte[] s_headerBytesScratch = new byte[prefixLength + dateTimeRLength + 2 * suffixLength];

    static DateHeader()
    {
        var utf8 = "Date: "u8;

        utf8.CopyTo(s_headerBytesMaster);
        utf8.CopyTo(s_headerBytesScratch);
        s_headerBytesMaster[suffixIndex] = (byte)'\r';
        s_headerBytesMaster[suffixIndex + 1] = (byte)'\n';
        s_headerBytesMaster[suffixIndex + 2] = (byte)'\r';
        s_headerBytesMaster[suffixIndex + 3] = (byte)'\n';
        s_headerBytesScratch[suffixIndex] = (byte)'\r';
        s_headerBytesScratch[suffixIndex + 1] = (byte)'\n';
        s_headerBytesScratch[suffixIndex + 2] = (byte)'\r';
        s_headerBytesScratch[suffixIndex + 3] = (byte)'\n';

        SetDateValues(DateTimeOffset.UtcNow);
        SyncDateTimer();
    }

    public static ReadOnlySpan<byte> HeaderBytes => s_headerBytesMaster;

    public static void SyncDateTimer()
    {
        s_timer.Change(1000, 1000);
    }
    private static void SetDateValues(DateTimeOffset value)
    {
        lock (s_headerBytesScratch)
        {
            if (!Utf8Formatter.TryFormat(value, s_headerBytesScratch.AsSpan(prefixLength), out var written, 'R'))
            {
                throw new Exception("date time format failed");
            }
            Debug.Assert(written == dateTimeRLength);
            (s_headerBytesScratch, s_headerBytesMaster) = (s_headerBytesMaster, s_headerBytesScratch);
        }
    }
}
