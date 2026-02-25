using System.Buffers;
using System.Diagnostics.Contracts;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text.Json;
using System.Text.Json.Serialization;
using zerg;
using zerg.Utils;
using zerg.Utils.UnmanagedMemoryManager;

namespace silverlight;

internal sealed class ConnectionHandler
{
    private readonly unsafe byte* _inflightData;
    private int _inflightTail;
    private readonly int _length;
    
    [ThreadStatic]
    private static Utf8JsonWriter? t_writer;
    private static readonly JsonContext SerializerContext = JsonContext.Default;
    
    private const string _jsonBody = "Hello, World!";
    private static ReadOnlySpan<byte> s_plainTextBody => "Hello, World!"u8;
    
    private static ReadOnlySpan<byte> s_headersJson => "HTTP/1.1 200 OK\r\nContent-Length:   \r\nServer: S\r\nContent-Type: application/json\r\n"u8;
    private static ReadOnlySpan<byte> s_headersPlainText => "HTTP/1.1 200 OK\r\nContent-Length:   \r\nServer: S\r\nContent-Type: text/plain\r\n"u8;

    public unsafe ConnectionHandler(int length = 1024 * 16)
    {
        _length = length;
        
        // Allocating an unmanaged byte slab to store inflight data
        _inflightData = (byte*)NativeMemory.AlignedAlloc((nuint)_length, 64);

        _inflightTail = 0;
    }

    // Zero allocation read and write example
    // No Peeking
    internal async Task HandleConnectionAsync(Connection connection)
    {
        try
        {
            while (true) // Outer loop, iterates everytime we read more data from the wire
            {
                var result = await connection.ReadAsync(); // Read data from the wire
                if (result.IsClosed)
                    break;
                
                if (HandleResult(connection, ref result))
                {
                    await connection.FlushAsync(); // Mark data to be ready to be flushed
                }

                // Reset connection's ManualResetValueTaskSourceCore<ReadResult>
                connection.ResetRead();
            }
        }
        catch (Exception e)
        {
            Console.WriteLine($"Exception --: {e}");
        }
        finally
        {
            unsafe { NativeMemory.AlignedFree(_inflightData); }
        }
    }

    private unsafe bool HandleResult(Connection connection, ref ReadResult result)
    {
        bool flushable;
        int advanced;

        UnmanagedMemoryManager[] rings = connection.GetAllSnapshotRingsAsUnmanagedMemory(result);
        int ringCount = rings.Length;

        if (ringCount == 0)
            return false;

        int oldInflightTail = _inflightTail;

        if (_inflightTail == 0)
        {
            flushable = ProcessRings(connection, rings, out advanced);
        }
        else
        {
            // Cold path
            UnmanagedMemoryManager[] mems = new UnmanagedMemoryManager[ringCount + 1];

            mems[0] = new(_inflightData, _inflightTail);

            for (int i = 1; i < ringCount + 1; i++)
                mems[i] = rings[i - 1];

            flushable = ProcessRings(connection, mems, out advanced);

            if (flushable)  // a request was handled so inflight data can be discarded
                _inflightTail = 0;
        }

        if (!flushable)
        {
            // No complete request found. Copy ring data to the inflight buffer
            // and return rings to the kernel.
            for (int i = 0; i < rings.Length; i++)
            {
                Buffer.MemoryCopy(
                    rings[i].Ptr,
                    _inflightData + _inflightTail,
                    _length - _inflightTail,
                    rings[i].Length);
                _inflightTail += rings[i].Length;
            }

            for (int i = 0; i < rings.Length; i++)
                connection.ReturnRing(rings[i].BufferId);

            return false;
        }

        // When inflight data was prepended, advanced includes those bytes.
        // Subtract them so advanced is relative to rings only.
        int ringAdvanced = advanced - oldInflightTail;
        int ringsTotalLength = CalculateRingsTotalLength(rings);

        if (ringAdvanced < ringsTotalLength)
        {
            var currentRingIndex = GetCurrentRingIndex(in ringAdvanced, rings, out var currentRingAdvanced);

            // Copy current ring unused data
            Buffer.MemoryCopy(
                rings[currentRingIndex].Ptr + currentRingAdvanced, // source
                _inflightData + _inflightTail, // destination
                _length - _inflightTail, // destinationSizeInBytes
                rings[currentRingIndex].Length - currentRingAdvanced); // sourceBytesToCopy

            _inflightTail += rings[currentRingIndex].Length - currentRingAdvanced;

            // Copy untouched rings data
            for (int i = currentRingIndex + 1; i < rings.Length; i++)
            {
                Buffer.MemoryCopy(
                    rings[i].Ptr, // source
                    _inflightData + _inflightTail, // destination
                    _length - _inflightTail, // destinationSizeInBytes
                    rings[i].Length); // sourceBytesToCopy

                _inflightTail += rings[i].Length;
            }
        }

        // Return the rings to the kernel, at this stage the request was either handled or the rings' data
        // has already been copied to the inflight buffer.
        for (int i = 0; i < rings.Length; i++) 
            connection.ReturnRing(rings[i].BufferId);
        
        return flushable;
    }
    
    [SkipLocalsInit][Pure][MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static unsafe bool ProcessRings(Connection connection, UnmanagedMemoryManager[] rings, out int advanced)
    {
        advanced = 0;

        int idx;
        bool flushable = false;

        ReadOnlySpan<byte> data = rings.Length == 1
            ? new ReadOnlySpan<byte>(rings[0].Ptr, rings[0].Length)
            : rings.ToReadOnlySequence().ToArray();

        while (true)
        {
            idx = data.IndexOf("\r\n\r\n"u8);
            if (idx == -1) return flushable;

            int idx4 = idx + 4;
            advanced += idx4;
            int space1 = data.IndexOf((byte)' ');
            if (space1 == -1) return flushable;
            int space2 = data[(space1 + 1)..].IndexOf((byte)' ');
            if (space2 <= 0) return flushable;

            ReadOnlySpan<byte> route = data[(space1 + 1)..(space1 + 1 + space2)];

            WriteResponse(connection, route[1] == (byte)'j');
            flushable = true;
            if (idx4 >= data.Length) break;

            data = data[idx4..];
        }

        return true;
    }

    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    private static void WriteResponse(Connection connection, bool json)
    {
        var tail = connection.WriteTail;
        int contentLength;

        if (json)
        {
            connection.Write(s_headersJson);
            connection.Write(DateHelper.HeaderBytes);

            var utf8JsonWriter = t_writer ??= new Utf8JsonWriter(connection, new JsonWriterOptions { SkipValidation = true });
            utf8JsonWriter.Reset(connection);
            JsonSerializer.Serialize(utf8JsonWriter, new JsonMessage { Message = _jsonBody }, SerializerContext.JsonMessage);

            contentLength = (int)utf8JsonWriter.BytesCommitted;
        }
        else
        {
            connection.Write(s_headersPlainText);
            connection.Write(DateHelper.HeaderBytes);
            connection.Write(s_plainTextBody);

            contentLength = s_plainTextBody.Length;
        }

        unsafe
        {
            byte* dst = connection.WriteBuffer + tail + 33;
            int tens = contentLength / 10;
            int ones = contentLength - tens * 10;

            dst[0] = (byte)('0' + tens);
            dst[1] = (byte)('0' + ones);
        }
    }
    
    private static int GetCurrentRingIndex(in int totalAdvanced, UnmanagedMemoryManager[] rings, out int currentRingAdvanced)
    {
        var total = 0;

        for (int i = 0; i < rings.Length; i++)
        {
            if (rings[i].Length + total >= totalAdvanced)
            {
                currentRingAdvanced = totalAdvanced - total;
                return i;
            }
            
            total += rings[i].Length;
        }

        currentRingAdvanced = -1;
        return -1;
    }

    private static int CalculateRingsTotalLength(UnmanagedMemoryManager[] rings)
    {
        var total = 0;
        for (int i = 0; i < rings.Length; i++) total += rings[i].Length;
        return total;
    }
}

public struct JsonMessage { public string Message { get; set; } }

[JsonSourceGenerationOptions(GenerationMode = JsonSourceGenerationMode.Serialization | JsonSourceGenerationMode.Metadata)]
[JsonSerializable(typeof(JsonMessage))]
public partial class JsonContext : JsonSerializerContext { }