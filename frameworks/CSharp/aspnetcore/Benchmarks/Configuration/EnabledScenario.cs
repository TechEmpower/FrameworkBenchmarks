// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

namespace Benchmarks.Configuration;

public class EnabledScenario
{
    public EnabledScenario(string name, IEnumerable<string> paths)
    {
        Name = name;
        Paths = paths;
    }

    public string Name { get; }

    public IEnumerable<string> Paths { get; }
}
