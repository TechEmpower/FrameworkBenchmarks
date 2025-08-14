using AkazawaYun.PRO7;
using MySql.Data.MySqlClient;
using System.Data.Common;

namespace AkazawaYun.FrameworkBenchmarks;

class Mysql : akzDbFactory
{
    protected override DbConnection NewConnection() => new MySqlConnection(ConString);
}
