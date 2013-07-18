using System;
using System.Collections.Generic;
using System.Linq;

using ServiceStack.Common.Web;
using ServiceStack.ServiceHost;
using ServiceStack.ServiceInterface;

using ServiceStackBenchmark.Model;

namespace ServiceStackBenchmark
{

    #region Hello World Services

    [Api("Test #1 (JSON serialization) using Service Stack")]
    [Route("/json", "GET")]
    public class JsonRequest { }

    public class JsonService : Service
    {
        public object Get(JsonRequest request)
        {
            var response = new { message = "Hello, World!" };
            return response;
        }
    }

    [Api("Test #6 (Plaintext) using Service Stack")]
    [Route("/plaintext", "GET")]
    public class PlainTextRequest { }

    public class PlainTextService : Service
    {
        public object Get(PlainTextRequest request)
        {
            var response = new HttpResult("Hello, World!", "text/plain");
            return response;
        }
    }

    #endregion

    #region MySQL Services

    [Api("Test #2 using Service Stack, ORMLite, and MySQL")]
    [Route("/mysql/db", "GET")]
    public class MySqlDbRequest : IReturn<World>
    { }

    [Api("Test #3 using Service Stack, ORMLite, and MySQL")]
    [Route("/mysql/queries/{queries}", "GET")]
    public class MySqlQueriesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    [Api("Test #4 using Service Stack, ORMLite, and MySQL")]
    [Route("/mysql/fortunes", "GET")]
    public class MySqlFortunesRequest : IReturn<List<Fortune>>
    { }

    [Api("Test #5 using Service Stack, ORMLite, and MySQL")]
    [Route("/mysql/updates/{queries}", "GET")]
    public class MySqlUpdatesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    /// <summary>Service Stack tests using MySQL provider and ORMLite</summary>
    public class MySqlService : Service
    {
        public IMySqlOrmLiteConnectionFactory dbFactory { get; set; }

        public object Get(MySqlDbRequest request)
        {
            using (var db = dbFactory.OpenDbConnection())
            {
                return new HttpResult(db.GetRandomWorld(new Random()), ContentType.Json);
            }
        }

        public object Get(MySqlQueriesRequest request)
        {
            using (var db = dbFactory.OpenDbConnection())
            {
                var worldCount = Math.Max(1, Math.Min(500, (int)request.queries)); // limit queries to be between 1 and 500 iterations
                return new HttpResult(db.GetRandomWorlds(worldCount, new Random()), ContentType.Json);
            }
        }

        [AddHeader(ContentType = ContentType.Html)]
        public object Get(MySqlFortunesRequest request)
        {
            var fortunes = new List<Fortune>();

            using (var db = dbFactory.OpenDbConnection())
            {
                fortunes = db.GetFortunes();
                fortunes.Add(new Fortune { id = 0, message = "Additional fortune added at request time." });
                fortunes.Sort();
            }

            // construct HTML page using template and return
            return FortuneMethods.ToHtml(fortunes);
        }

        public object Get(MySqlUpdatesRequest request)
        {
            using (var db = dbFactory.OpenDbConnection())
            {
                var worldCount = Math.Max(1, Math.Min(500, (int)request.queries)); // limit queries to be between 1 and 500 iterations
                return new HttpResult(db.UpdateRandomWorlds(worldCount, new Random()), ContentType.Json);
            }
        }
    }

    #endregion

    #region PostgreSQL Services

    [Api("Test #2 using Service Stack, ORMLite, and PostgreSQL")]
    [Route("/postgresql/db", "GET")]
    public class PostgreSqlDbRequest : IReturn<World>
    { }

    [Api("Test #3 using Service Stack, ORMLite, and PostgreSQL")]
    [Route("/postgresql/queries/{queries}", "GET")]
    public class PostgreSqlQueriesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    [Api("Test #4 using Service Stack, ORMLite, and PostgreSQL")]
    [Route("/postgresql/fortunes", "GET")]
    public class PostgreSqlFortunesRequest : IReturn<List<Fortune>>
    { }

    [Api("Test #5 using Service Stack, ORMLite, and PostgreSQL")]
    [Route("/postgresql/updates/{queries}", "GET")]
    public class PostgreSqlUpdatesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    /// <summary>Service Stack tests using PostgreSQL provider and ORMLite</summary>
    public class PostgreSqlService : Service
    {
        public IPostgreSqlOrmLiteConnectionFactory dbFactory { get; set; }

        public object Get(PostgreSqlDbRequest request)
        {
            using (var db = dbFactory.OpenDbConnection())
            {
                return new HttpResult(db.GetRandomWorld(new Random()), ContentType.Json);
            }
        }

        public object Get(PostgreSqlQueriesRequest request)
        {
            using (var db = dbFactory.OpenDbConnection())
            {
                var worldCount = Math.Max(1, Math.Min(500, (int)request.queries)); // limit queries to be between 1 and 500 iterations
                return new HttpResult(db.GetRandomWorlds(worldCount, new Random()), ContentType.Json);
            }
        }

        [AddHeader(ContentType = ContentType.Html)]
        public object Get(PostgreSqlFortunesRequest request)
        {
            var fortunes = new List<Fortune>();

            using (var db = dbFactory.OpenDbConnection())
            {
                fortunes = db.GetFortunes();
                fortunes.Add(new Fortune { id = 0, message = "Additional fortune added at request time." });
                fortunes.Sort();
            }

            // construct HTML page using template and return
            return FortuneMethods.ToHtml(fortunes);
        }

        public object Get(PostgreSqlUpdatesRequest request)
        {
            using (var db = dbFactory.OpenDbConnection())
            {
                var worldCount = Math.Max(1, Math.Min(500, (int)request.queries)); // limit queries to be between 1 and 500 iterations
                return new HttpResult(db.UpdateRandomWorlds(worldCount, new Random()), ContentType.Json);
            }
        }
    }

    #endregion

    #region Microsoft SQL Server Services

    [Api("Test #2 using Service Stack, ORMLite, and Microsoft SQL Server")]
    [Route("/sqlserver/db", "GET")]
    public class SqlServerDbRequest : IReturn<World>
    { }

    [Api("Test #3 using Service Stack, ORMLite, and Microsoft SQL Server")]
    [Route("/sqlserver/queries/{queries}", "GET")]
    public class SqlServerQueriesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    [Api("Test #4 using Service Stack, ORMLite, and Microsoft SQL Server")]
    [Route("/sqlserver/fortunes", "GET")]
    public class SqlServerFortunesRequest : IReturn<List<Fortune>>
    { }

    [Api("Test #5 using Service Stack, ORMLite, and Microsoft SQL Server")]
    [Route("/sqlserver/updates/{queries}", "GET")]
    public class SqlServerUpdatesRequest : IReturn<List<World>>
    {
        [ApiMember(Name = "queries", Description = "Number of Queries to Execute", DataType = "int", IsRequired = true)]
        [ApiAllowableValues("queries", 1, 500)]
        public int queries { get; set; }
    }

    /// <summary>Service Stack tests using Microsoft SQL Server provider and ORMLite</summary>
    public class SqlServerService : Service
    {
        public ISqlServerOrmLiteConnectionFactory dbFactory { get; set; }

        public object Get(SqlServerDbRequest request)
        {
            using (var db = dbFactory.OpenDbConnection())
            {
                return new HttpResult(db.GetRandomWorld(new Random()), ContentType.Json);
            }
        }

        public object Get(SqlServerQueriesRequest request)
        {
            using (var db = dbFactory.OpenDbConnection())
            {
                var worldCount = Math.Max(1, Math.Min(500, (int)request.queries)); // limit queries to be between 1 and 500 iterations
                return new HttpResult(db.GetRandomWorlds(worldCount, new Random()), ContentType.Json);
            }
        }

        [AddHeader(ContentType = ContentType.Html)]
        public object Get(SqlServerFortunesRequest request)
        {
            var fortunes = new List<Fortune>();

            using (var db = dbFactory.OpenDbConnection())
            {
                fortunes = db.GetFortunes();
                fortunes.Add(new Fortune { id = 0, message = "Additional fortune added at request time." });
                fortunes.Sort();
            }

            // construct HTML page using template and return
            return FortuneMethods.ToHtml(fortunes);
        }

        public object Get(SqlServerUpdatesRequest request)
        {
            using (var db = dbFactory.OpenDbConnection())
            {
                var worldCount = Math.Max(1, Math.Min(500, (int)request.queries)); // limit queries to be between 1 and 500 iterations
                return new HttpResult(db.UpdateRandomWorlds(worldCount, new Random()), ContentType.Json);
            }
        }
    }

    #endregion

}