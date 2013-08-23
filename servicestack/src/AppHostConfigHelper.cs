using System;
using System.Data;
using System.Linq;
using System.Configuration;
using System.Collections.Generic;
using System.Threading;
using ServiceStack.ServiceHost;
using ServiceStackBenchmark.Model;

namespace ServiceStackBenchmark
{
    public static class AppHostConfigHelper
    {
        public static bool InitMySQL(this Funq.Container container)
        {
            try
            {
                // Register the MySql Database Connection Factory
                var mySqlConnectionString = ConfigurationManager.ConnectionStrings["MySQL"];
                var mySqlFactory = new MySqlOrmLiteConnectionFactory(mySqlConnectionString.ConnectionString);
                mySqlFactory.DialectProvider.UseUnicode = true;
                container.Register<IMySqlOrmLiteConnectionFactory>(c => mySqlFactory);

                // Create needed tables in MySql Server if they do not exist
                using (var conn = mySqlFactory.OpenDbConnection())
                {
                    return conn.CreateWorldTable() && conn.CreateFortuneTable();
                }
            }
            catch (Exception ex)
            {
                // Unregister failed database connection factory
                container.Register<IMySqlOrmLiteConnectionFactory>(c => null);

                return false;
            }

        }

        public static bool InitPostgreSQL(this Funq.Container container)
        {
            try
            {
                // Register the PostgreSQL Database Connection Factory
                var postgreSqlConnectionString = ConfigurationManager.ConnectionStrings["PostgreSQL"];
                var postgreSqlFactory = new PostgreSqlOrmLiteConnectionFactory(postgreSqlConnectionString.ConnectionString);
                postgreSqlFactory.DialectProvider.UseUnicode = true;
                container.Register<IPostgreSqlOrmLiteConnectionFactory>(c => postgreSqlFactory);

                // Create needed tables in PostgreSql Server if they do not exist
                using (var conn = postgreSqlFactory.OpenDbConnection())
                {
                    return conn.CreateWorldTable() && conn.CreateFortuneTable();
                }
            }
            catch (Exception ex)
            {
                // Unregister failed database connection factory
                container.Register<IPostgreSqlOrmLiteConnectionFactory>(c => null);

                return false;
            }

        }

        public static bool InitSQLServer(this Funq.Container container)
        {
            try
            {
                // Register the Microsoft Sql Server Database Connection Factory
                var sqlServerConnectionString = ConfigurationManager.ConnectionStrings["SQLServer"];
                var sqlServerFactory = new SqlServerOrmLiteConnectionFactory(sqlServerConnectionString.ConnectionString);
                sqlServerFactory.DialectProvider.UseUnicode = true;
                container.Register<ISqlServerOrmLiteConnectionFactory>(c => sqlServerFactory);

                // Create needed tables in Microsoft Sql Server if they do not exist
                using (var conn = sqlServerFactory.OpenDbConnection())
                {
                    return conn.CreateWorldTable() && conn.CreateFortuneTable();
                }
            }
            catch (Exception ex)
            {
                // Unregister failed database connection factory
                container.Register<ISqlServerOrmLiteConnectionFactory>(c => null);

                return false;
            }

        }

        public static void InitDatabaseRoutes(this Funq.Container container, IServiceRoutes routes)
        {
            if (container.InitMySQL())
            {
                routes.Add<MySqlDbRequest>("/mysql/db", "GET");
                routes.Add<MySqlQueriesRequest>("/mysql/queries/{queries}", "GET");
                routes.Add<MySqlFortunesRequest>("/mysql/fortunes", "GET");
                routes.Add<MySqlUpdatesRequest>("/mysql/updates/{queries}", "GET");
                routes.Add<MySqlCachedDbRequest>("/mysql/cached/db", "GET");
            }

            if (container.InitPostgreSQL())
            {
                routes.Add<PostgreSqlDbRequest>("/postgresql/db", "GET");
                routes.Add<PostgreSqlQueriesRequest>("/postgresql/queries/{queries}", "GET");
                routes.Add<PostgreSqlFortunesRequest>("/postgresql/fortunes", "GET");
                routes.Add<PostgreSqlUpdatesRequest>("/postgresql/updates/{queries}", "GET");
                routes.Add<PostgreSqlCachedDbRequest>("/postgresql/cached/db", "GET");
            }

            if (container.InitSQLServer())
            {
                routes.Add<SqlServerDbRequest>("/sqlserver/db", "GET");
                routes.Add<SqlServerQueriesRequest>("/sqlserver/queries/{queries}", "GET");
                routes.Add<SqlServerFortunesRequest>("/sqlserver/fortunes", "GET");
                routes.Add<SqlServerUpdatesRequest>("/sqlserver/updates/{queries}", "GET");
                routes.Add<SqlServerCachedDbRequest>("/sqlserver/cached/db", "GET");
            }
        }

        public static Feature GetDisabledFeatures()
        {
            try
            {
                var disabled = ConfigurationManager.AppSettings.Get("DisabledFeatures");

                Feature d;
                if (Enum.TryParse(disabled, true, out d))
                    return d;

                return Feature.None;
            }
            catch
            {
                return Feature.None;
            }

        }

        /// <summary>
        /// Method to config the Minimum and Maximum number of Worker Threads per Logical Processor Count.
        /// </summary>
        /// <remarks>the Completion Port Threads are set to their defaults as there is no IO concerrency in our app</remarks>
        public static void ConfigThreadPool()
        {
            int sysMinWorkerThreads, sysMinCompletionPortThreads;
            ThreadPool.GetMinThreads(out sysMinWorkerThreads, out sysMinCompletionPortThreads);

            int sysMaxWorkerThreads, sysMaxCompletionPortThreads;
            ThreadPool.GetMaxThreads(out sysMaxWorkerThreads, out sysMaxCompletionPortThreads);

            int newMinWorkerThreadsPerCPU = Math.Max(1, Convert.ToInt32(ConfigurationManager.AppSettings["minWorkerThreadsPerLogicalProcessor"] ?? "1"));
            int newMaxWorkerThreadsPerCPU = Math.Max(newMinWorkerThreadsPerCPU, Convert.ToInt32(ConfigurationManager.AppSettings["maxWorkerThreadsPerLogicalProcessor"] ?? "100"));

            var minWorkerThreads = Environment.ProcessorCount * newMinWorkerThreadsPerCPU;
            ThreadPool.SetMinThreads(minWorkerThreads, sysMinCompletionPortThreads);

            var maxWorkerThreads = Environment.ProcessorCount * newMaxWorkerThreadsPerCPU;
            ThreadPool.SetMaxThreads(maxWorkerThreads, sysMaxCompletionPortThreads);
        }
    }
}
