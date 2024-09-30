using System.Collections.Concurrent;
using System.Data.Odbc;

namespace appMpower.Orm.Data
{
   internal static class DbConnectionsKeyed
   {
      private static bool _maxConnectionsCreated = false;
      private static short _createdConnections = 0;
      private static short _maxConnections = 500;

      private static ConcurrentStack<(int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand>)> _connectionsStack = new();
      private static ConcurrentQueue<TaskCompletionSource<(int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand>)>> _waitingQueue = new();

      internal static async Task<(int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand> KeyedOdbcCommands)> GetConnectionBase(string connectionString)
      {
         (int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand> KeyedOdbcCommands) dbConnectionBase;

         if (!_connectionsStack.TryPop(out dbConnectionBase))
         {
            if (_maxConnectionsCreated)
            {
               dbConnectionBase = await GetDbConnectionBaseAsync();
            }
            else
            {
               _createdConnections++;
               dbConnectionBase = (Number: _maxConnections, OdbcConnection: new OdbcConnection(connectionString), KeyedOdbcCommands: new Dictionary<string, OdbcCommand>());

               if (_createdConnections == _maxConnections) _maxConnectionsCreated = true;

               //Console.WriteLine("opened connection number: " + dbConnectionBase._number);
            }
         }

         return dbConnectionBase;
      }

      internal static void Release((int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand> KeyedOdbcCommands) dbConnectionBase)
      {
         TaskCompletionSource<(int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand>)> taskCompletionSource;

         if (_waitingQueue.TryDequeue(out taskCompletionSource))
         {
            taskCompletionSource.SetResult(dbConnectionBase);
         }
         else
         {
            _connectionsStack.Push(dbConnectionBase);
         }
      }

      private static Task<(int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand>)> GetDbConnectionBaseAsync()
      {
         var taskCompletionSource = new TaskCompletionSource<(int Number, OdbcConnection OdbcConnection, Dictionary<string, OdbcCommand>)>(TaskCreationOptions.RunContinuationsAsynchronously);

         _waitingQueue.Enqueue(taskCompletionSource);
         return taskCompletionSource.Task;
      }
   }
}