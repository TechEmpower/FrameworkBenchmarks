using System;

using ServiceStack.OrmLite;
using ServiceStack.OrmLite.PostgreSQL;

namespace ServiceStackBenchmark
{
    public class PostgreSqlOrmLiteConnectionFactory : OrmLiteConnectionFactory, IPostgreSqlOrmLiteConnectionFactory
    {
        public PostgreSqlOrmLiteConnectionFactory(string s) : base(s, PostgreSQLDialectProvider.Instance) {
            this.DialectProvider.NamingStrategy = new LowercaseNamingStrategy();
        }
    }

    public class LowercaseNamingStrategy : OrmLiteNamingStrategyBase
    {
        public override string GetTableName(string name)
        {
            return name.ToLower();
        }

        public override string GetColumnName(string name)
        {
            return name.ToLower();
        }

    }

}
