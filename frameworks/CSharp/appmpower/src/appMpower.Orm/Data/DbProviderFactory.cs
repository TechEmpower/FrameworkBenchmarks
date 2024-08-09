using System.Data;

namespace appMpower.Orm.Data
{
   public static class DbProviderFactory
   {
      public static string ConnectionString; 

      public static void SetConnectionString()
      {
         if (Constants.Dbms == Dbms.MySQL)
         {
            ConnectionString = "Driver={MariaDB};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;Pooling=false;OPTIONS=67108864;FLAG_FORWARD_CURSOR=1;sslmode=DISABLED;charset=UTF8;"; 
         }
         else
         {
            ConnectionString = "Driver={PostgreSQL};Server=tfb-database;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false;sslmode=disable";
            //TODOLOCAL
            //ConnectionString = "Driver={PostgreSQL};Server=localhost;Database=hello_world;Uid=benchmarkdbuser;Pwd=benchmarkdbpass;UseServerSidePrepare=1;Pooling=false";
         }
      }
   }
}