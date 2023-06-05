// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Buffers.Text;
using System.Diagnostics;
using System.Text;

namespace PlatformBenchmarks;

/// <summary>
/// Manages the generation of the date header value.
/// </summary>
internal static class DateHeader
{
    const int prefixLength = 8; // "\r\nDate: ".Length
    const int dateTimeRLength = 29; // Wed, 14 Mar 2018 14:20:00 GMT
    const int suffixLength = 2; // crlf
    const int suffixIndex = dateTimeRLength + prefixLength;

    private static readonly Timer s_timer = new(_ => SetDateValues(DateTimeOffset.UtcNow), null, 1000, 1000);

    private static byte[] s_headerBytesMaster = new byte[prefixLength + dateTimeRLength + 2 * suffixLength];
    private static byte[] s_headerBytesScratch = new byte[prefixLength + dateTimeRLength + 2 * suffixLength];

    static DateHeader()
    {
        var utf8 = "\r\nDate: "u8;

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

    public static void SyncDateTimer() => s_timer.Change(1000, 1000);

    public static ReadOnlySpan<byte> HeaderBytes => s_headerBytesMaster;

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
