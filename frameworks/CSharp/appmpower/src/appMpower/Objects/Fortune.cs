using System;

namespace appMpower.Objects
{
   public struct Fortune : IComparable<Fortune>, IComparable
   {
      public Fortune(int id, string message)
      {
         Id = id;
         Message = message;
      }

      public int Id { get; set; }

      public string Message { get; set; }

      public int CompareTo(object obj) => throw new InvalidOperationException("The non-generic CompareTo should not be used");

      // Performance critical, using culture insensitive comparison
      public int CompareTo(Fortune other) => string.CompareOrdinal(Message, other.Message);
   }
}