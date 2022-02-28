// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using System.Runtime.InteropServices;

namespace PlatformBenchmarks;

[StructLayout(LayoutKind.Sequential, Size = 8)]
public struct World
{
    public int Id { get; set; }

    public int RandomNumber { get; set; }
}
