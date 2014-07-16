using System;

using ServiceStack.OrmLite;
using ServiceStack.OrmLite.SqlServer;

namespace ServiceStackBenchmark
{
    public class SqlServerOrmLiteConnectionFactory : OrmLiteConnectionFactory, ISqlServerOrmLiteConnectionFactory
    {
        public SqlServerOrmLiteConnectionFactory(string s) : base(s, SqlServerOrmLiteDialectProvider.Instance) { }
    }
}
