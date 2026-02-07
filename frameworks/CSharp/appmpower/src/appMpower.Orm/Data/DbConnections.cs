#if ODBC
using System.Collections.Concurrent;

namespace appMpower.Orm.Data
{
   internal static class DbConnections
   {
      private static short _createdConnections = 0;

      private static ConcurrentStack<(int Number, System.Data.Common.DbConnection DbConnection, ConcurrentStack<System.Data.Common.DbCommand> DbCommands)> _connectionsStack = new();
      
      internal static (int Number, System.Data.Common.DbConnection DbConnection, ConcurrentStack<System.Data.Common.DbCommand> DbCommands) GetConnectionBase()
      {
         (int Number, System.Data.Common.DbConnection DbConnection, ConcurrentStack<System.Data.Common.DbCommand> DbCommands) dbConnectionBase;

         if (!_connectionsStack.TryPop(out dbConnectionBase))
         {
            _createdConnections++;
            dbConnectionBase = (Number: _createdConnections, 
                                DbConnection: DbFactory.GetConnection(), 
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
#endif