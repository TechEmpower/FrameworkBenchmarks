using System.Collections.Concurrent;
using System.Collections.Generic;

namespace appMpower.Orm.Data
{
   internal static class DbConnectionsKeyed
   {
      private static short _createdConnections = 0;

      private static ConcurrentStack<(int Number, System.Data.Common.DbConnection DbConnection, Dictionary<string, System.Data.Common.DbCommand>)> _connectionsStack = new();

      internal static (int Number, System.Data.Common.DbConnection DbConnection, Dictionary<string, System.Data.Common.DbCommand> KeyedDbCommands) GetConnectionBase(string connectionString)
      {
         (int Number, System.Data.Common.DbConnection DbConnection, Dictionary<string, System.Data.Common.DbCommand> KeyedDbCommands) dbConnectionBase;

         if (!_connectionsStack.TryPop(out dbConnectionBase))
         {
            _createdConnections++;
            using System.Data.Common.DbConnection dbConnection = DbFactory.Instance.CreateConnection();
            dbConnection.ConnectionString = connectionString; 

            dbConnectionBase = (Number: _createdConnections, 
                                DbConnection: dbConnection, 
                                KeyedDbCommands: new Dictionary<string, System.Data.Common.DbCommand>());
         }

         return dbConnectionBase;
      }

      internal static void Release((int Number, System.Data.Common.DbConnection DbConnection, Dictionary<string, System.Data.Common.DbCommand> KeyedDbCommands) dbConnectionBase)
      {
         _connectionsStack.Push(dbConnectionBase);
      }
   }
}