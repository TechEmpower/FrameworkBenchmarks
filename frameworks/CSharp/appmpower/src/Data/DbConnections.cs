using System.Collections.Concurrent;
using System.Threading.Tasks;

namespace appMpower.Data
{
   public static class DbConnections
   {
      private static bool _connectionsCreated = false;
      private static short _createdConnections = 0;
      private static short _maxConnections = 250;

      private static ConcurrentStack<InternalConnection> _stack = new();
      private static ConcurrentQueue<TaskCompletionSource<InternalConnection>> _waitingQueue = new();

      public static async Task<InternalConnection> GetConnection(string connectionString)
      {
         InternalConnection internalConnection = null;

         if (_connectionsCreated)
         {
            if (!_stack.TryPop(out internalConnection))
            {
               internalConnection = await GetDbConnectionAsync();
            }

            return internalConnection;
         }
         else
         {
            internalConnection = new InternalConnection();
            internalConnection.DbConnection = new System.Data.Odbc.OdbcConnection(connectionString);

            _createdConnections++;

            if (_createdConnections == _maxConnections) _connectionsCreated = true;

            internalConnection.Number = _createdConnections;
            internalConnection.DbCommands = new ConcurrentDictionary<string, DbCommand>();
            //Console.WriteLine("opened connection number: " + dbConnection.Number);

            return internalConnection;
         }
      }

      public static Task<InternalConnection> GetDbConnectionAsync()
      {
         var taskCompletionSource = new TaskCompletionSource<InternalConnection>(TaskCreationOptions.RunContinuationsAsynchronously);

         _waitingQueue.Enqueue(taskCompletionSource);
         return taskCompletionSource.Task;
      }

      public static void Release(InternalConnection internalConnection)
      {
         TaskCompletionSource<InternalConnection> taskCompletionSource;

         if (_waitingQueue.TryDequeue(out taskCompletionSource))
         {
            taskCompletionSource.SetResult(internalConnection);
         }
         else
         {
            _stack.Push(internalConnection);
         }
      }
   }
}