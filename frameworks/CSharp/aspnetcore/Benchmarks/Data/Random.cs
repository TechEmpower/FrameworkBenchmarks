// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System;

namespace Benchmarks.Data
{
    public class DefaultRandom : IRandom
    {
        private readonly Random _random = new Random();

        public int Next(int minValue, int maxValue)
        {
            return _random.Next(minValue, maxValue);
        }
    }
}
