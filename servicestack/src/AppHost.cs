using System;
using System.Linq;
using System.Configuration;
using System.Collections.Generic;

using ServiceStack;
using ServiceStack.Api.Swagger;
using ServiceStack.Common;
using ServiceStack.Common.Web;
using ServiceStack.ServiceHost;
using ServiceStack.WebHost.Endpoints;
using ServiceStack.WebHost.Endpoints.Formats;

using ServiceStackBenchmark.Model;

namespace ServiceStackBenchmark
{

	public class AppHost : AppHostBase
	{
		
		public AppHost() : base("ServiceStackBenchmark", typeof(AppHost).Assembly) { }

        #region Connection Strings

        private static readonly ConnectionStringSettings mySqlConnectionString = ConfigurationManager.ConnectionStrings["MySQL"];

        private static readonly ConnectionStringSettings sqlServerConnectionString = ConfigurationManager.ConnectionStrings["SQLServer"];

        private static readonly ConnectionStringSettings postgreSqlConnectionString = ConfigurationManager.ConnectionStrings["PostgreSQL"];

        private static readonly ConnectionStringSettings mongoDbConnectionString = ConfigurationManager.ConnectionStrings["MongoDB"];

        #endregion

        public override void Configure(Funq.Container container)
		{
			ServiceStack.Text.JsConfig.EmitCamelCaseNames = true;

            // Remove some unused features that by default are included
            Plugins.RemoveAll(p => p is CsvFormat);
            Plugins.RemoveAll(p => p is MetadataFeature);

            // Add plugins
            Plugins.Add(new SwaggerFeature());

            SetConfig(new EndpointHostConfig
            {
                DefaultRedirectPath = "/swagger-ui/index.html", // default to the Swagger page
                DefaultContentType = ContentType.Json,
                WriteErrorsToResponse = false,
                EnableFeatures = Feature.All.Remove(GetDisabledFeatures()), // disable features specified in Web.Config (i.e. Soap, Metadata, etc.)
                AppendUtf8CharsetOnContentTypes = new HashSet<string> { ContentType.Html },
            });

            // Initialize Databases
            InitDatabases(container);

		}

        private static void InitDatabases(Funq.Container container)
        {
            // Register the MySql Database Connection Factory
            var mySqlFactory = new MySqlOrmLiteConnectionFactory(mySqlConnectionString.ConnectionString);
            mySqlFactory.DialectProvider.UseUnicode = true;
            container.Register<IMySqlOrmLiteConnectionFactory>(c => mySqlFactory);

            // Create needed tables in MySql Server if they do not exist
            try
            {
                using (var conn = mySqlFactory.OpenDbConnection())
                {
                    conn.CreateWorldTable();
                    conn.CreateFortuneTable();
                }
            }
            catch
            { }

            // Register the PostgreSQL Database Connection Factory
            var postgreSqlFactory = new PostgreSqlOrmLiteConnectionFactory(postgreSqlConnectionString.ConnectionString);
            postgreSqlFactory.DialectProvider.UseUnicode = true;
            container.Register<IPostgreSqlOrmLiteConnectionFactory>(c => postgreSqlFactory);

            // Create needed tables in PostgreSql Server if they do not exist
            try
            {
                using (var conn = postgreSqlFactory.OpenDbConnection())
                {
                    conn.CreateWorldTable();
                    conn.CreateFortuneTable();
                }
            }
            catch
            { }

            // Register the Microsoft Sql Server Database Connection Factory
            var sqlServerFactory = new SqlServerOrmLiteConnectionFactory(sqlServerConnectionString.ConnectionString);
            sqlServerFactory.DialectProvider.UseUnicode = true;
            container.Register<ISqlServerOrmLiteConnectionFactory>(c => sqlServerFactory);

            // Create needed tables in Microsoft Sql Server if they do not exist
            try
            {
                using (var conn = sqlServerFactory.OpenDbConnection())
                {
                    conn.CreateWorldTable();
                    conn.CreateFortuneTable();
                }
            }
            catch
            { }
        }

        private static Feature GetDisabledFeatures()
        {
            var disabled = ConfigurationManager.AppSettings.Get("DisabledFeatures");

            Feature d;
            if (!string.IsNullOrWhiteSpace(disabled) && Enum.TryParse(disabled, true, out d))
                return d;

            return Feature.None;
        }

	}
}