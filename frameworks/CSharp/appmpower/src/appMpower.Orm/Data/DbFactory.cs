using System;
using System.Data.Common;

namespace appMpower.Orm.Data
{
   public static class DbFactory
   {
      public static string ConnectionString; 

#if ADO
      public static DbProviderFactory Instance = MySqlConnector.MySqlConnectorFactory.Instance;
#else
      public static DbProviderFactory Instance = System.Data.Odbc.OdbcFactory.Instance;
#endif

      public static void SetConnectionString()
      {
         if (Constants.Dbms == Dbms.MySQL)
         {
#if ADO
            ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=1024;SslMode=None;ConnectionReset=false;ConnectionIdlePingTime=900;ConnectionIdleTimeout=0;AutoEnlist=false;DefaultCommandTimeout=0;ConnectionTimeout=0;IgnorePrepare=false;";
            Console.WriteLine("hey ado mysql");
#else
            ConnectionString = "Driver={MariaDB};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;Pooling=false;OPTIONS=67108864;FLAG_FORWARD_CURSOR=1;sslmode=DISABLED;CharSet=utf8;"; 
            Console.WriteLine("hey odbc mysql");
#endif
         }
         else
         {
            ConnectionString = "Driver={PostgreSQL};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false;sslmode=disable";
         }
      }
   }
}