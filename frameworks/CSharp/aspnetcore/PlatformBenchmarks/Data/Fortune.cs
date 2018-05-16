// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;

namespace PlatformBenchmarks
{
    public class Fortune : IComparable<Fortune>, IComparable
    {
        public int Id { get; set; }

        public string Message { get; set; }
        
        public int CompareTo(object obj)
        {
            return CompareTo((Fortune)obj);
        }

        public int CompareTo(Fortune other)
        {
            // Performance critical, using culture insensitive comparison
            return String.CompareOrdinal(Message, other.Message);
        }
    }
}
