using System;

using ServiceStack.OrmLite;
using ServiceStack.OrmLite.MySql;

namespace ServiceStackBenchmark
{
    public class MySqlOrmLiteConnectionFactory : OrmLiteConnectionFactory, IMySqlOrmLiteConnectionFactory
    {
        public MySqlOrmLiteConnectionFactory(string s) : base(s, MySqlDialectProvider.Instance) { }
    }
}
