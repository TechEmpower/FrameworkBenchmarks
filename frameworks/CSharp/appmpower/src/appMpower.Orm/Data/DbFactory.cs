using System;
using System.Data.Common;

namespace appMpower.Orm.Data
{
   public static class DbFactory
   {
      public static string ConnectionString; 

#if ADO
   #if MYSQL
      public static DbProviderFactory Instance = MySqlConnector.MySqlConnectorFactory.Instance;
   #else
      public static DbProviderFactory Instance = Npgsql.NpgsqlFactory.Instance; 
   #endif 
#else
      public static DbProviderFactory Instance = System.Data.Odbc.OdbcFactory.Instance;
#endif

      public static System.Data.Common.DbConnection GetConnection()
      {
          System.Data.Common.DbConnection dbConnection = Instance.CreateConnection();
          dbConnection.ConnectionString = ConnectionString;

          return dbConnection; 
      }

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
#if ADO
            ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3";
            Console.WriteLine("hey ado postgresql");
#else
            ConnectionString = "Driver={PostgreSQL};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false;sslmode=disable";
            Console.WriteLine("hey odbc postgresql");
#endif
         }
      }
   }
}