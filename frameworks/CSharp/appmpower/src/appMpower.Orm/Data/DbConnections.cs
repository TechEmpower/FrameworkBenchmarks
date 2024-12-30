using System.Collections.Concurrent;
using System.Data.Odbc;

namespace appMpower.Orm.Data
{
   internal static class DbConnections
   {
      private static bool _maxConnectionsCreated = false;
      private static short _createdConnections = 0;
      private static short _maxConnections = 500;

      private static ConcurrentStack<(int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands)> _connectionsStack = new();
      private static ConcurrentQueue<TaskCompletionSource<(int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands)>> _waitingQueue = new();
      
      internal static async Task<(int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands)> GetConnectionBase(string connectionString)
      {
         (int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands) dbConnectionBase;

         if (!_connectionsStack.TryPop(out dbConnectionBase))
         {
            if (_maxConnectionsCreated)
            {
               dbConnectionBase = await GetDbConnectionBaseAsync();
            }
            else
            {
               _createdConnections++;
               dbConnectionBase = (Number: _maxConnections, OdbcConnection: new OdbcConnection(connectionString), OdbcCommands: new ConcurrentStack<OdbcCommand>());

               if (_createdConnections == _maxConnections) _maxConnectionsCreated = true;

               //Console.WriteLine("opened connection number: " + dbConnectionBase._number);
            }
         }

         return dbConnectionBase;
      }

      internal static void Release((int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands) dbConnectionBase)
      {
         TaskCompletionSource<(int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands)> taskCompletionSource;

         if (_waitingQueue.TryDequeue(out taskCompletionSource))
         {
            taskCompletionSource.SetResult(dbConnectionBase);
         }
         else
         {
            _connectionsStack.Push(dbConnectionBase);
         }
      }

      private static Task<(int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands)> GetDbConnectionBaseAsync()
      {
         var taskCompletionSource = new TaskCompletionSource<(int Number, OdbcConnection OdbcConnection, ConcurrentStack<OdbcCommand> OdbcCommands)>(TaskCreationOptions.RunContinuationsAsynchronously);

         _waitingQueue.Enqueue(taskCompletionSource);
         return taskCompletionSource.Task;
      }
   }
}