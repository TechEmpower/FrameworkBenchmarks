// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

namespace Benchmarks.Configuration;

public sealed class AppSettings
{
    public string ConnectionString { get; set; }

    public DatabaseServer Database { get; set; } = DatabaseServer.None;
}
