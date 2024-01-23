using System;

namespace PlatformBenchmarks
{
   public sealed class CacheKey : IEquatable<CacheKey>
   {
      private readonly int _value;

      public CacheKey(int value)
          => _value = value;

      public bool Equals(CacheKey key)
          => key._value == _value;

      public override bool Equals(object obj)
          => ReferenceEquals(obj, this);

      public override int GetHashCode()
          => _value;

      public override string ToString()
          => _value.ToString();
   }
}