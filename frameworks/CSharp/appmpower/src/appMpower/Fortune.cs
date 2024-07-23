using System;

namespace appMpower
{
   public readonly struct Fortune : IComparable<Fortune>, IComparable
   {
      public Fortune(int id, string message)
      {
         Id = id;
         Message = message;
      }

      public int Id { get; }

      public string Message { get; }

      public int CompareTo(object obj) => throw new InvalidOperationException("The non-generic CompareTo should not be used");

      // Performance critical, using culture insensitive comparison
      public int CompareTo(Fortune other) => string.CompareOrdinal(Message, other.Message);
   }
}