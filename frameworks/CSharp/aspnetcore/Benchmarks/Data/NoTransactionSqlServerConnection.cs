// Copyright (c) .NET Foundation. All rights reserved. 
// Licensed under the Apache License, Version 2.0. See License.txt in the project root for license information. 

using System.Data;
using System.Threading;
using System.Threading.Tasks;
using Microsoft.EntityFrameworkCore.Infrastructure;
using Microsoft.EntityFrameworkCore.Storage;
using Microsoft.EntityFrameworkCore.Storage.Internal;
using Microsoft.Extensions.Logging;

namespace Benchmarks.Data
{
    class NoTransactionSqlServerConnection : SqlServerConnection
    {
        public NoTransactionSqlServerConnection(IDbContextOptions options, ILogger<SqlServerConnection> logger)
            : base(options, logger)
        {
        }

        public override Task<IDbContextTransaction> BeginTransactionAsync(
            IsolationLevel isolationLevel, CancellationToken cancellationToken = new CancellationToken())
            => Task.FromResult<IDbContextTransaction>(new FakeTransaction());

        public override IDbContextTransaction BeginTransaction(IsolationLevel isolationLevel) 
            => new FakeTransaction();

        private class FakeTransaction : IDbContextTransaction
        {
            public void Dispose()
            {
            }

            public void Commit()
            {
            }

            public void Rollback()
            {
            }
        }
    }
}