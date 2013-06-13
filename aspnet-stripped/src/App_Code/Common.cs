using System;
using System.Configuration;
using System.Data;
using System.Data.Common;
using System.Web;

public class Common
{
    public static DbConnection CreateConnection(HttpRequest request)
    {
        // Never tried this with any other provider
        string providerName = "sqlserver";
        ConnectionStringSettings connectionSettings = ConfigurationManager.ConnectionStrings[providerName];
        DbProviderFactory factory = DbProviderFactories.GetFactory(connectionSettings.ProviderName);
        DbConnection connection = factory.CreateConnection();
        connection.ConnectionString = connectionSettings.ConnectionString;
        return connection;
    }

    public static int GetQueries(HttpRequest request) {
        int queries = 1;
        string queriesString = request.QueryString["queries"];
        if (queriesString != null) {
            // If this fails to parse, queries will be set to zero.
            int.TryParse(queriesString, out queries);
            queries = Math.Max(1, Math.Min(500, queries));
        }
        return queries;
    }
}