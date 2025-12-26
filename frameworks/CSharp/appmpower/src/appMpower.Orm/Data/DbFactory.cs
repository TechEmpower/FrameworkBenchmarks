using System;
using System.Data.Common;

namespace appMpower.Orm.Data
{
   public static class DbFactory
   {
      public static string ConnectionString;
      public static DbProviderFactory Instance;

      public static System.Data.Common.DbConnection GetConnection(string? connectionString = null)
      {
          System.Data.Common.DbConnection dbConnection = Instance.CreateConnection();
          dbConnection.ConnectionString = connectionString ?? ConnectionString;

          return dbConnection; 
      }

      public static void SetConnectionString()
      {
         if (Constants.DbProvider == DbProvider.ODBC)
         {
            if (Constants.Dbms == Dbms.MySQL)
            {
               ConnectionString = "Driver={MariaDB};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;Pooling=false;OPTIONS=67108864;FLAG_FORWARD_CURSOR=1;sslmode=DISABLED;CharSet=utf8;"; 
               Console.WriteLine("hey odbc mysql");
            }
            else
            {
               ConnectionString = "Driver={PostgreSQL};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false;sslmode=disable";
               Console.WriteLine("hey odbc postgresql");
            }
         }         
         else if (Constants.DbProvider == DbProvider.ADO)
         {
            if (Constants.Dbms == Dbms.MySQL)
            {
               ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=1024;SslMode=None;ConnectionReset=false;ConnectionIdlePingTime=900;ConnectionIdleTimeout=0;AutoEnlist=false;DefaultCommandTimeout=0;ConnectionTimeout=0;IgnorePrepare=false;";
               Console.WriteLine("hey ado mysql");
            }
            else
            {
               ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;Maximum Pool Size=1024;NoResetOnClose=true;Enlist=false;Max Auto Prepare=3;";
               Console.WriteLine("hey ado postgresql");
            }
         }
      }

      public static void SetInstance()
      {
         if (Constants.DbProvider == DbProvider.ODBC)
         {
            Instance = System.Data.Odbc.OdbcFactory.Instance;
         }         
         else if (Constants.DbProvider == DbProvider.ADO)
         {
            if (Constants.Dbms == Dbms.MySQL)
            {
               Instance = MySqlConnector.MySqlConnectorFactory.Instance;
            }
            else
            {
               Instance = Npgsql.NpgsqlFactory.Instance; 
            }
         }
      }
   }
}