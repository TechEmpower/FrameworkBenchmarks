using System.Collections.Concurrent;
using System.Data.Odbc;

namespace appMpower.Orm.Data
{
   internal static class DbConnections
   {
      private static short _createdConnections = 0;

      private static ConcurrentStack<(int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands)> _connectionsStack = new();
      
      internal static (int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands) GetConnectionBase(string connectionString)
      {
         (int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands) dbConnectionBase;

         if (!_connectionsStack.TryPop(out dbConnectionBase))
         {
            _createdConnections++;
            dbConnectionBase = (Number: _createdConnections, OdbcConnection: new OdbcConnection(connectionString), OdbcCommands: new ConcurrentStack<OdbcCommand>());
         }

         return dbConnectionBase;
      }

      internal static void Release((int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands) dbConnectionBase)
      {
         _connectionsStack.Push(dbConnectionBase);
      }
   }
}