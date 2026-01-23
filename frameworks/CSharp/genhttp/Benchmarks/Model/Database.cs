namespace Benchmarks.Model;

using Npgsql;

public static class Database
{
    
    public static readonly NpgsqlDataSource DataSource = BuildDataSource();

    private static NpgsqlDataSource BuildDataSource()
    {
        var connectionString = Environment.GetEnvironmentVariable("DB_CONNECTION");
        
        return new NpgsqlSlimDataSourceBuilder(connectionString).EnableArrays().Build();
    }
    
}
