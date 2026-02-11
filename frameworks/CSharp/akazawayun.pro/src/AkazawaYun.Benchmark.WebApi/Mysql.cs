using AkazawaYun.PRO7;
using MySql.Data.MySqlClient;
using System.Data.Common;

namespace AkazawaYun.Benchmark.WebApi;

class Mysql : akaDbFactory
{
    protected override DbConnection NewConnection() => new MySqlConnection(ConString);
}
