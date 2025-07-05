using System.Collections.Concurrent;
using System.Data.Odbc;

namespace appMpower.Orm.Data
{
   internal static class DbConnectionsKeyed
   {
      private static short _createdConnections = 0;

      private static ConcurrentStack<(int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand>)> _connectionsStack = new();

      internal static (int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand> KeyedOdbcCommands) GetConnectionBase(string connectionString)
      {
         (int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand> KeyedOdbcCommands) dbConnectionBase;

         if (!_connectionsStack.TryPop(out dbConnectionBase))
         {
            _createdConnections++;
            dbConnectionBase = (Number: _createdConnections, OdbcConnection: new OdbcConnection(connectionString), KeyedOdbcCommands: new Dictionary<string, OdbcCommand>());
         }

         return dbConnectionBase;
      }

      internal static void Release((int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand> KeyedOdbcCommands) dbConnectionBase)
      {
         _connectionsStack.Push(dbConnectionBase);
      }
   }
}