namespace Benchmarks.Model;

using Npgsql;

public static class Database
{
    private static readonly string ConnectionString = Environment.GetEnvironmentVariable("DB_CONNECTION");

    public static NpgsqlConnection Connection() => new(ConnectionString);

}
