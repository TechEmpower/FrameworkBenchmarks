using System.Collections.Concurrent;

namespace appMpower.Orm.Data
{
   internal static class DbConnections
   {
      private static short _createdConnections = 0;

      private static ConcurrentStack<(int Number, System.Data.Common.DbConnection DbConnection, ConcurrentStack<System.Data.Common.DbCommand> DbCommands)> _connectionsStack = new();
      
      internal static (int Number, System.Data.Common.DbConnection DbConnection, ConcurrentStack<System.Data.Common.DbCommand> DbCommands) GetConnectionBase(string connectionString)
      {
         (int Number, System.Data.Common.DbConnection DbConnection, ConcurrentStack<System.Data.Common.DbCommand> DbCommands) dbConnectionBase;

         if (!_connectionsStack.TryPop(out dbConnectionBase))
         {
            _createdConnections++;

            System.Data.Common.DbConnection dbConnection = DbFactory.Instance.CreateConnection();
            dbConnection.ConnectionString = connectionString; 

            dbConnectionBase = (Number: _createdConnections, 
                                DbConnection: dbConnection, 
                                DbCommands: new ConcurrentStack<System.Data.Common.DbCommand>());
         }

         return dbConnectionBase;
      }

      internal static void Release((int Number, System.Data.Common.DbConnection DbConnection, ConcurrentStack<System.Data.Common.DbCommand> DbCommands) dbConnectionBase)
      {
         _connectionsStack.Push(dbConnectionBase);
      }
   }
}