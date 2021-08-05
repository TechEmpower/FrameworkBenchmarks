namespace appMpower
{
   public static class ConnectionStrings
   {
#if MYSQL
      public const string OdbcConnection = "Driver={MariaDB};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;Pooling=false;OPTIONS=67108864;FLAG_FORWARD_CURSOR=1"; 
      public const string OdbcConnectionJapanese = "Driver={MySQL};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;Pooling=false;FLAG_FORWARD_CURSOR=1";
#else
      public const string OdbcConnection = "Driver={PostgreSQL};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false";
#endif
   }
}