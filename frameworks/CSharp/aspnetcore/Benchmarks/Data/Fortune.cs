// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace Benchmarks.Data;

[Table("fortune")]
public sealed class Fortune : IComparable<Fortune>, IComparable
{
    [Column("id")]
    public int Id { get; set; }

    [Column("message")]
    [StringLength(2048)]
    [Required]
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
