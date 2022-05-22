// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using LinqToDB;
using LinqToDB.Configuration;
using LinqToDB.Data;

namespace Benchmarks.Data;

public sealed class ApplicationDataConnection : DataConnection
{
    public ApplicationDataConnection(LinqToDBConnectionOptions<ApplicationDataConnection> options)
        : base(options)
    {
    }

    public ITable<World> World => this.GetTable<World>();
    public ITable<Fortune> Fortune => this.GetTable<Fortune>();
}
