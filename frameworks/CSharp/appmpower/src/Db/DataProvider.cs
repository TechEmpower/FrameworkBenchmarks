namespace appMpower.Db
{
   public static class DataProvider
   {
#if MYSQL
      public const string ConnectionString = "Driver={MariaDB};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;Pooling=false;OPTIONS=67108864;FLAG_FORWARD_CURSOR=1"; 
#elif ADO
      public const string ConnectionString = "Server=tfb-database;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=18;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000";
      //public const string ConnectionString = "Server=localhost;Database=hello_world;User Id=benchmarkdbuser;Password=benchmarkdbpass;SSL Mode=Disable;Maximum Pool Size=18;Enlist=false;Max Auto Prepare=4;Multiplexing=true;Write Coalescing Buffer Threshold Bytes=1000";
#else
      public const string ConnectionString = "Driver={PostgreSQL};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false";
      //public const string ConnectionString = "Driver={PostgreSQL};Server=localhost;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false";
#endif
   }
}