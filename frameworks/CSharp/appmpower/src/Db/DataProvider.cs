namespace appMpower.Db
{
   public static class DataProvider
   {
#if MYSQL
      public const bool IsOdbcConnection = true; 
      public const string ConnectionString = "Driver={MariaDB};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;Pooling=false;OPTIONS=67108864;FLAG_FORWARD_CURSOR=1"; 
#else
      public const bool IsOdbcConnection = true;
      public const string ConnectionString = "Driver={PostgreSQL};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false";
#endif
   }
}