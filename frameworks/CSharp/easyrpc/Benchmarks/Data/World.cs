// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Runtime.InteropServices;

namespace Benchmarks.Data
{
    [StructLayout(LayoutKind.Sequential, Size = 8)]
    public struct World
    {
        public int id { get; set; }

        public int randomNumber { get; set; }
    }
}
