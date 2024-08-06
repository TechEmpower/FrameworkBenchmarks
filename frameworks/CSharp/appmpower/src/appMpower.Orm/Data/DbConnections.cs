using System.Collections.Concurrent;

namespace appMpower.Orm.Data
{
   public static class DbConnections
   {
      private static short _createdConnections = 0;
      private static ConcurrentStack<DbConnection> _connectionsStack = new();

      public static DbConnection GetConnection(string connectionString)
      {
         DbConnection popDbConnection; 

         if (!_connectionsStack.TryPop(out popDbConnection))
         {
            popDbConnection = new DbConnection();
            popDbConnection._odbcConnection = new System.Data.Odbc.OdbcConnection(connectionString);

            _createdConnections++;
            popDbConnection._number = _createdConnections;
            Console.WriteLine("connection created: " + _createdConnections.ToString());
         }

         return popDbConnection; 
      }


      public static void GetConnection(string connectionString, DbConnection dbConnection)
      {
         DbConnection popDbConnection = null;

         if (_connectionsStack.TryPop(out popDbConnection))
         {
            dbConnection._odbcConnection = popDbConnection._odbcConnection; 
            dbConnection._odbcCommands = popDbConnection._odbcCommands;
            dbConnection._number = popDbConnection._number; 
         }
         else
         {
            dbConnection._odbcConnection = new System.Data.Odbc.OdbcConnection(connectionString);

            _createdConnections++;
            dbConnection._number = _createdConnections;

            Console.WriteLine("connection created: " + _createdConnections.ToString());
         }
      }

      public static void Release(DbConnection dbConnection)
      {
         _connectionsStack.Push(dbConnection);
      }
   }
}