// Copyright (c) .NET Foundation. All rights reserved.
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information.

using LinqToDB;
using LinqToDB.Data;
using LinqToDB.Mapping;

namespace Benchmarks.Data;

public sealed class ApplicationDataConnection : DataConnection
{
    public static readonly MappingSchema Mappings;

    static ApplicationDataConnection()
    {
        Mappings = new FluentMappingBuilder(new("TechEmpower"))
            .Entity<World>()
                .HasTableName("world")
                .Property(e => e.Id)
                    .HasColumnName("id")
                    .IsPrimaryKey()
                .Property(e => e.RandomNumber)
                    .HasColumnName("randomnumber")
            .Entity<Fortune>()
                .HasTableName("fortune")
                .Property(e => e.Id)
                    .HasColumnName("id")
                    .IsPrimaryKey()
                .Property(e => e.Message)
                    .HasColumnName("message")
                    .IsNotNull()
                    .HasLength(2048)
            .Build().MappingSchema;
    }

    public ApplicationDataConnection(DataOptions options)
        : base(options)
    {
    }

    public ITable<World> World => this.GetTable<World>();
    public ITable<Fortune> Fortune => this.GetTable<Fortune>();
}
