using System;
using System.Buffers;
using System.IO.Pipelines;

namespace PlatformBenchmarks
{
   public struct WriterAdapter : IBufferWriter<byte>
   {
      public PipeWriter Writer;

      public WriterAdapter(PipeWriter writer)
          => Writer = writer;

      public void Advance(int count)
          => Writer.Advance(count);

      public Memory<byte> GetMemory(int sizeHint = 0)
          => Writer.GetMemory(sizeHint);

      public Span<byte> GetSpan(int sizeHint = 0)
          => Writer.GetSpan(sizeHint);
   }
}