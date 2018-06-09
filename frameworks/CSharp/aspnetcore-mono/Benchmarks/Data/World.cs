// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.ComponentModel.DataAnnotations.Schema;
using System.Runtime.InteropServices;

namespace Benchmarks.Data
{
    [Table("world")]
    public class World
    {
        [Column("id")]
        public int Id { get; set; }

        [Column("randomnumber")]
        public int RandomNumber { get; set; }
    }

    [StructLayout(LayoutKind.Sequential, Size = 8)]
    public struct WorldRaw
    {
        public int Id { get; set; }

        public int RandomNumber { get; set; }
    }
}
