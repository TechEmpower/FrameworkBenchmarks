using System.Data;

namespace appMpowerAot.Data
{
   public static class DbProviderFactory
   {
      public static string ConnectionString; 

      public static void SetConnectionString()
      {
         if (Constants.Dbms == Dbms.MySQL)
         {
            ConnectionString = "Driver={MariaDB};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;Pooling=false;OPTIONS=67108864;FLAG_FORWARD_CURSOR=1"; 
         }
         else if (Constants.DbProvider == DbProvider.ADO)
         {
            ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=0;Maximum Pool Size=18;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000";
            //ConnectionString = "Server=localhost;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=18;NoResetOnClose=true;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000"; 
         }
         else
         {
            ConnectionString = "Driver={PostgreSQL};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false";
            //ConnectionString = "Driver={PostgreSQL};Server=localhost;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false";
         }
      }
   }
}