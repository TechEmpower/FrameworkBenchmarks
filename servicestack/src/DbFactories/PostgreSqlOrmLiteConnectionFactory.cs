using System;

using ServiceStack.OrmLite;
using ServiceStack.OrmLite.PostgreSQL;

namespace ServiceStackBenchmark
{
    public class PostgreSqlOrmLiteConnectionFactory : OrmLiteConnectionFactory, IPostgreSqlOrmLiteConnectionFactory
    {
        public PostgreSqlOrmLiteConnectionFactory(string s) : base(s, PostgreSQLDialectProvider.Instance) { }
    }
}
