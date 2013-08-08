using System;

using ServiceStack.OrmLite;
using ServiceStack.OrmLite.Sqlite;

namespace ServiceStackBenchmark
{
    public class SQLiteOrmLiteConnectionFactory : OrmLiteConnectionFactory, ISQLiteOrmLiteConnectionFactory
    {
        // NOTE: since we are using an In-Memory database, we can not close the connection because it will destroy the database
        public SQLiteOrmLiteConnectionFactory(string s) : base(s, false, SqliteOrmLiteDialectProvider.Instance, true) 
        { }
    }
}
