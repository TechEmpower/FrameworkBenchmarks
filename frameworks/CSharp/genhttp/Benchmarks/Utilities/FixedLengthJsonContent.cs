using System.Text.Json;

using GenHTTP.Api.Protocol;

using Benchmarks.Tests;

namespace Benchmarks.Utilities;

public sealed class FixedLengthJsonContent : IResponseContent
{
    private readonly MemoryStream _buffer = new();

    public ulong? Length => (ulong)_buffer.Length;

    public FixedLengthJsonContent(JsonResult result)
    {
        JsonSerializer.SerializeAsync(_buffer, result);
    }
    
    public ValueTask<ulong?> CalculateChecksumAsync() => throw new NotImplementedException();

    public ValueTask WriteAsync(Stream target, uint bufferSize)
    {
        _buffer.Seek(0, SeekOrigin.Begin);
        
        _buffer.CopyTo(target);
        
        return ValueTask.CompletedTask;
    }

}
