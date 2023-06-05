// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

namespace PlatformBenchmarks;

public readonly struct FortuneUtf8 : IComparable<FortuneUtf8>, IComparable
{
    public FortuneUtf8(int id, byte[] message)
    {
        Id = id;
        Message = message;
    }

    public int Id { get; }

    public byte[] Message { get; }

    public int CompareTo(object obj) => throw new InvalidOperationException("The non-generic CompareTo should not be used");

    // Performance critical, using culture insensitive comparison
    public int CompareTo(FortuneUtf8 other) => Message.AsSpan().SequenceCompareTo(other.Message.AsSpan());
}
