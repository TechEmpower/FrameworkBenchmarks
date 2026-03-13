using System.Buffers.Text;

namespace silverlight;

/// <summary>
/// Manages the generation of the date header value.
/// </summary>
public static class DateHelper
{
    private const int PrefixLength = 6; // "Date: ".Length
    private const int DateTimeRLength = 29; // Wed, 14 Mar 2018 14:20:00 GMT
    private const int SuffixLength = 2; // crlf
    private const int SuffixIndex = DateTimeRLength + PrefixLength;

    private static readonly Timer STimer = new((s) => {
        SetDateValues(DateTimeOffset.UtcNow);
    }, null, 1000, 1000);

    private static byte[] _sHeaderBytesMaster = new byte[PrefixLength + DateTimeRLength + 2 * SuffixLength];
    private static byte[] _sHeaderBytesScratch = new byte[PrefixLength + DateTimeRLength + 2 * SuffixLength];

    static DateHelper()
    {
        var utf8 = "Date: "u8;

        utf8.CopyTo(_sHeaderBytesMaster);
        utf8.CopyTo(_sHeaderBytesScratch);
        _sHeaderBytesMaster[SuffixIndex] = (byte)'\r';
        _sHeaderBytesMaster[SuffixIndex + 1] = (byte)'\n';
        _sHeaderBytesMaster[SuffixIndex + 2] = (byte)'\r';
        _sHeaderBytesMaster[SuffixIndex + 3] = (byte)'\n';
        _sHeaderBytesScratch[SuffixIndex] = (byte)'\r';
        _sHeaderBytesScratch[SuffixIndex + 1] = (byte)'\n';
        _sHeaderBytesScratch[SuffixIndex + 2] = (byte)'\r';
        _sHeaderBytesScratch[SuffixIndex + 3] = (byte)'\n';

        SetDateValues(DateTimeOffset.UtcNow);
        SyncDateTimer();
    }

    private static void SyncDateTimer() => STimer.Change(1000, 1000);
    public static ReadOnlySpan<byte> HeaderBytes => _sHeaderBytesMaster;

    private static void SetDateValues(DateTimeOffset value)
    {
        lock (_sHeaderBytesScratch)
        {
            if (!Utf8Formatter.TryFormat(value, _sHeaderBytesScratch.AsSpan(PrefixLength), out var written, 'R'))
                throw new Exception("date time format failed");
            
            //Debug.Assert(written == dateTimeRLength);
            (_sHeaderBytesScratch, _sHeaderBytesMaster) = (_sHeaderBytesMaster, _sHeaderBytesScratch);
        }
    }
}